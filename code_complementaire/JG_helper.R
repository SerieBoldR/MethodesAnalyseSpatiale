
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Brant test pour vglm (adapte du package brant) ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print.testresult.vglm <- function(model,X2,df.v,by.var) {
  p.values = pchisq(X2,df.v,lower.tail=FALSE)
  woit <- grepl("Intercept",names(coef(model)),fixed=T) == F
  if(by.var){
    var.names = unlist(strsplit(as.character(formula(model))[3],split=" \\+ "))
  }else{
    var.names = names(coef(model)[woit])
  }
  # longest name
  longest.char = max(nchar(var.names))
  n.tabs = ceiling(longest.char/7)
  n.tabs = ifelse(n.tabs<2,2,n.tabs)
  cat(paste0(rep("-",28+8*n.tabs),collapse = ""),"\n")
  cat(paste0("Test for",paste0(rep("\t",n.tabs-1),collapse = ""),"X2\tdf\tprobability"),"\n")
  cat(paste0(rep("-",28+8*n.tabs),collapse = ""),"\n")
  cat(paste0("Omnibus",paste0(rep("\t",n.tabs),collapse = ""),round(X2[1],digits=2),"\t",df.v[1],"\t",round(p.values[1],digits=2)))
  cat("\n")
  for(i in 1:length(var.names)){
    name = var.names[i]
    tabs.sub = ceiling(nchar(name)/7)-1
    cat(paste0(name,paste0(rep("\t",n.tabs-tabs.sub),collapse = ""),round(X2[i+1],digits=2),"\t",df.v[i+1],"\t",round(p.values[i+1],digits=2),"\n"))
  }
  cat(paste0(rep("-",28+8*n.tabs),collapse = ""),"\n\n")
  cat("H0: Parallel Regression Assumption holds")
  result.matrix = matrix(c(X2, df.v, p.values), ncol = 3)
  rownames(result.matrix) = c("Omnibus", var.names)
  colnames(result.matrix) = c("X2","df","probability")
  result.matrix
}

getCombiCoefs.vglm <- function(model){
  classes = sapply(model@model, class)
  factors = ifelse(classes[2:length(classes)]!="numeric",T,F)
  f = i = var = 1
  woit <- grepl("Intercept",names(coef(model)),fixed=T) == F
  len <- length(coef(model)[woit])
  result = data.frame(i=1:len,var=NA)
  for(factor in factors){
    if(factor){
      n = length(unlist(model@xlevels[f]))
      for(j in 1:(n-1)){
        result[i,"var"] = var
        i = i + 1
      }
      var = var + 1
      f = f + 1
    }else{
      result[i,"var"] = var
      var = var + 1
      i = i + 1
    }
  }
  return(result)
}

brant.vglm <- function(model,by.var=F){
  temp.data = model@model
  y_name = as.character(formula(model))[2]
  x_names = as.character(formula(model))[3]
  y = as.numeric(temp.data[[y_name]])
  temp.data$y = y

  x.variables = strsplit(x_names," \\+ ")[[1]]
  x.factors = c()
  for(name in x.variables){
    if(!is.numeric(temp.data[,name])){
      x.factors = c(x.factors,name)
    }
  }
  if(length(x.factors)>0){
    tab = table(data.frame(temp.data[,y_name],temp.data[,x.factors]))
    count0 = sum(tab==0)
  }else{
    count0 = 0
  }


  J = max(y,na.rm=T)

  woit <- grepl("Intercept",names(coef(model)),fixed=T) == F

  K = length(coef(model)[woit])
  for(m in 1:(J-1)){
    temp.data[[paste0("z",m)]] = ifelse(y>m,1,0)
  }
  binary.models = list()
  beta.hat = matrix(NA,nrow=J-1,ncol=K+1,byrow=T)
  var.hat = list()
  for(m in 1:(J-1)){
    mod = glm(paste0("z",m," ~ ",x_names),data=temp.data, family="binomial")
    binary.models[[paste0("model",m)]] = mod
    beta.hat[m,] = coef(mod)
    var.hat[[m]] = vcov(mod)
  }

  X.temp = model@model[2:length(model@model)]
  X = matrix(1,nrow=length(X.temp[,1]),ncol=1)
  for(var in X.temp){
    if(is.numeric(var)){
      X = cbind(X,var)
    }
    if(is.character(var)){
      var = as.factor(var)
    }
    if(is.factor(var)){
      for(level in levels(var)[2:length(levels(var))]){
        X = cbind(X,ifelse(var==level,1,0))
      }
    }
  }
  zeta <- model@coefficients[woit==F] * -1
  tau = matrix(zeta,nrow=1,ncol=J-1,byrow=T)
  pi.hat = matrix(NA,nrow=length(model@model[,1]),ncol=J-1,byrow=T)
  for(m in 1:(J-1)){
    pi.hat[,m] = binary.models[[m]]$fitted.values
  }


  varBeta = matrix(NA,nrow = (J-1)*K, ncol = (J-1)*K)
  for(m in 1:(J-2)){
    for(l in (m+1):(J-1)){
      Wml = Matrix::Diagonal(x=pi.hat[,l] - pi.hat[,m]*pi.hat[,l])
      Wm = Matrix::Diagonal(x=pi.hat[,m] - pi.hat[,m]*pi.hat[,m])
      Wl = Matrix::Diagonal(x=pi.hat[,l] - pi.hat[,l]*pi.hat[,l])
      Xt = t(X)
      varBeta[((m-1)*K+1):(m*K),((l-1)*K+1):(l*K)] = as.matrix((solve(Xt %*% Wm %*% X)%*%(Xt %*% Wml %*% X)%*%solve(Xt %*% Wl %*% X))[-1,-1])
      varBeta[((l-1)*K+1):(l*K),((m-1)*K+1):(m*K)] = varBeta[((m-1)*K+1):(m*K),((l-1)*K+1):(l*K)]
    }
  }

  betaStar = c()
  for(m in 1:(J-1)){
    betaStar = c(betaStar,beta.hat[m,-1])
  }
  for(m in 1:(J-1)){
    varBeta[((m-1)*K+1):(m*K),((m-1)*K+1):(m*K)] = var.hat[[m]][-1,-1]
  }

  I = diag(1,K)
  E0 = diag(0,K)
  for(i in 1:(J-2)){
    for(j in 1:(J-1)){
      if(j == 1){
        temp = I
      }else if(j == i+1){
        temp = cbind(temp,-I)
      }else{
        temp = cbind(temp,E0)
      }
    }
    if(i==1){
      D = temp
    }else{
      D = rbind(D,temp)
    }
  }
  X2 = t(D%*%betaStar) %*% solve(D %*% varBeta %*% t(D)) %*% (D %*% betaStar)
  df.v = (J-2)*K

  if(by.var){
    combinations = getCombiCoefs.vglm(model)
    for(v in unique(combinations$var)){
      k = subset(combinations,var==v)$i
      s = c()
      df.v.temp = 0
      for(e in k){
        s = c(s,seq(from=e,to=K*(J-1),by=K))
        df.v.temp = df.v.temp + J-2
      }
      s = sort(s)
      Ds = D[,s]
      Ds = Ds[which(!apply(Ds==0,1,all)),]
      if(!is.null(dim(Ds)))
        X2 = c(X2,t(Ds%*%betaStar[s]) %*% solve(Ds %*% varBeta[s,s] %*% t(Ds)) %*% (Ds %*% betaStar[s]))
      else
        X2 = c(X2,t(Ds%*%betaStar[s]) %*% solve(Ds %*% varBeta[s,s] %*% t(t(Ds))) %*% (Ds %*% betaStar[s]))
      df.v = c(df.v,df.v.temp)
    }
  }else{
    for(k in 1:K){
      s = seq(from=k,to=K*(J-1),by=K)
      Ds = D[,s]
      Ds = Ds[which(!apply(Ds==0,1,all)),]
      if(!is.null(dim(Ds)))
        X2 = c(X2,t(Ds%*%betaStar[s]) %*% solve(Ds %*% varBeta[s,s] %*% t(Ds)) %*% (Ds %*% betaStar[s]))
      else
        X2 = c(X2,t(Ds%*%betaStar[s]) %*% solve(Ds %*% varBeta[s,s] %*% t(t(Ds))) %*% (Ds %*% betaStar[s]))
      df.v = c(df.v,J-2)
    }
  }

  result.matrix = print.testresult.vglm(model,X2,df.v,by.var)
  if(count0!=0){
    warning(paste0(count0," combinations in table(dv,ivs) do not occur. Because of that, the test results might be invalid."))
  }
  result.matrix
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### parallel likelihood ratio test pour vglm ordinal ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parallel.likelihoodtest.vglm <- function(model, verbose = TRUE){

  ## recuperer les variable de l'equation
  formule <- as.formula(as.character(model@terms))
  xvars <- strsplit(as.character(formule)[[3]]," + ", fixed = T)[[1]]

  if(verbose){
    pb <- txtProgressBar(min = 0, max = length(xvars), style = 3)
  }
  i<-1
  test_results <- lapply(xvars,function(x){
    formule2 <- as.formula(paste(FALSE, "~ 1 +",x))
    model2 <- vglm(formule,
            family = cumulative(link="logitlink",parallel = formule2, reverse = TRUE),
            data = model@model)
    test <- anova.vglm(model2,model, type = "I")
    values <- list(
      "variable non parallele" = x,
      "AIC" = round(AIC(model2)),
      "loglikelihood" = round(logLik(model2)),
      "p.val loglikelihood ratio test" = test$`Pr(>Chi)`[[2]])
    if(verbose){
      setTxtProgressBar(pb, i)
    }

    i<<- i+1
    return(values)
  })

  tableau_final <- as.data.frame(t(matrix(unlist(test_results), nrow=length(unlist(test_results[1])))))
  names(tableau_final) <- c("variable non parallele", "AIC", "loglikelihood", "p.val loglikelihood ratio test")
  ## ajuster les valeur de p
  tableau_final[["p.val loglikelihood ratio test"]] <- round(p.adjust(tableau_final[["p.val loglikelihood ratio test"]], method = "fdr"),3)
  return(tableau_final)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Analyse de type 3, modele multinomial ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

AnalyseType3<-function(modele, data, fixed_vars = NULL, verbose = TRUE){
  logModeleComplet <- -2*logLik(modele)          # valeur du loglikehood pour le mod?le complet
  results <- list()  # liste vide qui comprendra les resultats
  all_vars <- strsplit(as.character(modele@terms), split = " ~ ")
  vardep <- all_vars[[1]][[1]]
  varsindep <- strsplit(all_vars[[1]][[2]], split = " + ", fixed = T)[[1]]
  if(is.null(fixed_vars) == F){
    testing_vars <- varsindep[varsindep %in% fixed_vars == F]
  }else{
    testing_vars <- varsindep
  }
  i <- 1
  if(verbose){
    pb <- txtProgressBar(min = 0, max = length(testing_vars), style = 3)
  }

  for(x1 in testing_vars){
    # R?cup?ration de la liste des variables ind?pendantes moins chaque variable d?pendante
    listvarindep = ""
    for(x2 in varsindep){
      if(x1 != x2){
        listvarindep <- paste(listvarindep, "+", x2)
      }
    }
    listvarindep <-  substr(listvarindep,3,nchar(listvarindep))

    # Construction du mod?le',
    formule <- as.formula(paste(vardep, " ~ ", listvarindep))
    model2 <- vglm(formule,
                   family = multinomial(parallel = FALSE),
                   data = data)
    test <- anova(model2,modele, type = "I")
    values <- list(
      "removed" = x1,
      "AIC" = round(AIC(model2)),
      "loglike" = round(-2* logLik(model2)),
      "sign" = round(test$`Pr(>Chi)`[[2]],4))
    results[[length(results) + 1]] <- values
    if(verbose){
      setTxtProgressBar(pb, i)
    }
    i <- i + 1
  }

  cat("*************************************", "\n")
  cat("Type 3 Analysis of Effects", "\n")
  cat("*************************************", "\n")
  cat("AIC model complet : ", round(AIC(modele)), "\n")
  cat("loglikelihood model complet : ", round(logModeleComplet), "\n")
  tableau_final <- as.data.frame(t(matrix(unlist(results), nrow=length(unlist(results[1])))))
  names(tableau_final) <- c("variable retiree",
                            "AIC", "loglikelihood", "p.val"
  )
  print(tableau_final)
  return(tableau_final)

}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### pseudo R2 ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rsqs <- function(loglike.full, loglike.null,full.deviance, null.deviance, nb.params, n){

  explained_dev <- 1-(full.deviance / null.deviance)

  K <- nb.params

  r2_faddenadj <- 1- (loglike.full - K) / loglike.null

  Lm <- loglike.full
  Ln <- loglike.null
  Rcs <- 1 - exp((-2/n) * (Lm-Ln))
  Rn <- Rcs / (1-exp(2*Ln/n))
  return(
    list("deviance expliquee" = explained_dev,
         "MacFadden ajuste" = r2_faddenadj,
         "Cox and Snell" = Rcs,
         "Nagelkerke" = Rn
    )
  )
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Afficher une belle matrice de confusion ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nice_confusion_matrix <- function(yreal, ypred){
  library(caret)
  ## generation de la matrice avec caret
  info <- confusionMatrix(as.factor(ypred),as.factor(yreal))

  ##pimper le tout
  mat <- info[[2]]
  rs <- rowSums(mat)
  rp <- round(rowSums(mat) / sum(mat) * 100,1)
  cs <- colSums(mat)
  cp <- round(colSums(mat) / sum(mat) * 100,1)
  mat2 <- cbind(mat,rs,rp)
  mat3 <- rbind(mat2,c(cs,sum(mat),NA),c(cp,NA,NA))

  rowsnames <- c(paste(colnames(mat),"(predit)"),"Total", "%")
  colsnames <- c("",paste(colnames(mat),"(reel)"),"Total", "%")

  mat4 <- cbind(rowsnames, mat3)
  mat5 <- rbind(colsnames, mat4)
  #print(kable(mat5,row.names = F))

  ## calcule des indicateurs pour chaque categorie
  precision <- diag(mat) / rowSums(mat)
  rappel <- diag(mat) / colSums(mat)
  F1 <- 2*((precision*rappel)/(precision + rappel))

  macro_scores <- c(weighted.mean(precision,colSums(mat)),
                    weighted.mean(rappel,colSums(mat)),
                    weighted.mean(F1,colSums(mat)))

  final_table <- rbind(cbind(precision,rappel,F1),macro_scores)
  final_table <- rbind(final_table, c(info[[3]][[2]],NA,NA), c(info[[3]][[6]],NA,NA))
  rnames <- c(rownames(mat),"macro","Kappa","Valeur de p  (precision > NIR)")
  final_table <- cbind(rnames,round(final_table,2))

  #print(kable(final_table, row.names =F))
  return(list("confusion_matrix" = mat5,
         "indicators" = final_table))

}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Afficher les coeff de models (glm, vglm) ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## fonction pour cleaner les colonnes

clean_columns <- function(tableau, digits){
  for(i in 1:length(digits)){
    d <- digits[[i]]
    if(is.na(d)==F){
      col <- tableau[,i]
      #nettoyer les 0
      test1 <- col == "0"
      col[test1] <- paste("<0.",paste(rep("0",(d-1)),collapse=""),"1",sep="")
      #nettoyer les arrondis trop intenses
      col <- sapply(col,function(t){
        if (grepl(".",t,fixed = T)){
          dig <- strsplit(t,".",fixed=T)[[1]][[2]]
          if(nchar(dig) == d){
            return(t)
          }else{
            dif <- d - nchar(dig)
            newt <- paste(t,paste(rep("0",dif),collapse=""),sep="")
          }
          return(newt)
        }else if (t == "1"){
          newt <- paste("1.",paste(rep("0",d),collapse=""),sep="")
        }else{
          return(t)
        }
      })
      tableau[,i] <- col
    }
  }
  return(tableau)
}

sign_col <- function(tableau){
  i<-match("val .p", colnames(tableau))
  col <- tableau[,i]
  sign <- sapply(col, function(j){
    if(j == "--"){
      return("--")
    }else if (j == ""){
      return("")
    }else{
      num <- as.numeric(
        gsub(",", ".",
             gsub("<","",j, fixed=T)
              ,fixed=T)
        )
      if(num<=0.001){
        return("***")
      }else if (num <= 0.01){
        return("**")
      }else if (num <= 0.05){
        return("*")
      }else if (num <= 0.1){
        return(".")
      }else {
        return("")
      }
    }
  })
  return(sign)
}

## fonction generale
build_table <- function(model, confid = T, sign = T, coef_digits = 2, std_digits = 2, z_digits = 2, p_digits = 3, OR_digits = 3, robust_se = NULL ){

  if (class(model)[[1]]=="vglm"){
    tableau <- build_table.vglm(model, confid = confid, coef_digits = coef_digits, std_digits = std_digits, z_digits = z_digits, p_digits = p_digits, OR_digits = p_digits)
  }else if(class(model)[[1]] %in% c("glm","lm")){
    tableau <- build_table.glm(model, confid = confid, coef_digits = coef_digits, std_digits = std_digits, z_digits = z_digits, p_digits = p_digits, OR_digits = p_digits, robust_se = robust_se)
  }else if(class(model)[[1]] == "gam"){
    tableau <- build_table.gam(model, confid = confid, coef_digits = coef_digits, std_digits = std_digits, z_digits = z_digits, p_digits = p_digits, OR_digits = p_digits)
  }

  if(sign){
    if ("matrix" %in% class(tableau)){
      newcol <- sign_col(tableau)
      new_names <- c(colnames(tableau),"Signif. codes")
      tableau <- cbind(tableau, newcol)
      colnames(tableau) <- new_names
      rownames(tableau) <- NULL
    }else {
      tableau <- lapply(tableau, function(t1){
        newcol <- sign_col(t1)
        new_names <- c(colnames(t1),"Signif. codes")
        t1 <- cbind(t1, newcol)
        colnames(t1) <- new_names
        rownames(t1) <- NULL
        return(t1)
      })
    }
  }

  return(tableau)

}

## pour un glm
build_table.glm <- function(model, confid = T, coef_digits = 2, std_digits = 2, z_digits = 2, p_digits = 3, OR_digits = 3, robust_se = NULL){
  ## extraction des elements principaux
  base_table <- summary(model)$coefficients

  if(is.null(robust_se) == F){
    covModel <- vcovHC(model, type = robust_se)
    stdErrRobuste <- sqrt(diag(covModel))
    base_table[,2] <- stdErrRobuste
    base_table[,3] <- base_table[,1] / stdErrRobuste
    base_table[,4] <- 2 * pnorm(abs(base_table[,3]), lower.tail = FALSE)
  }

  ## calcule des intervalles de confiance sur les coeffs
  if(confid){
    if (is.null(robust_se)){
      base_table <- cbind(base_table, round(confint(model),coef_digits))
    }else{
      c1 <- base_table[,1] + 1.96 * stdErrRobuste
      c2 <- base_table[,1] - 1.96 * stdErrRobuste
      base_table <- cbind(base_table, round(cbind(c2,c1),coef_digits))
    }

  }

  ## si fonction de lien = logit : OR
  if(class(model)=='glm'){
    if (model$family$link == "logit"){
      base_table <- cbind(base_table[,1], round(exp(base_table[,1]),OR_digits) , base_table[,2:ncol(base_table)])
      if(confid){
        n <- ncol(base_table)
        base_table <- cbind(base_table, round(exp(base_table[,c(n-1,n)]), OR_digits))
      }
    }else if (model$family$link == "log"){
      base_table <- cbind(base_table[,1], round(exp(base_table[,1]),coef_digits),base_table[,2:ncol(base_table)])
      if (confid){
        n <- ncol(base_table)
        base_table <- cbind(base_table, round(exp(base_table[,c(n-1,n)]), coef_digits))
      }
    }
  }


  ## gerer les arrondis
  base_table[,1] <- round(base_table[,1], coef_digits)
  i<-match("Std. Error", colnames(base_table))
  base_table[,i] <- round(base_table[,i], std_digits)

  testt <- i<-match("t value", colnames(base_table))

  if(is.na(testt)){
    i<-match("z value", colnames(base_table))
    base_table[,i] <- round(base_table[,i], z_digits)
    i<-match("Pr(>|z|)", colnames(base_table))
    base_table[,i] <- round(base_table[,i], p_digits)
  }else{
    i<-match("t value", colnames(base_table))
    base_table[,i] <- round(base_table[,i], z_digits)
    i<-match("Pr(>|t|)", colnames(base_table))
    base_table[,i] <- round(base_table[,i], p_digits)
  }

  # formatting the values
  base_table <- format(base_table, big.mark  = " ", decimal.mark = ",")

  params_names <- as.character(model$terms)
  params_names <- params_names[3:length(params_names)]
  params_names <- strsplit(params_names," + ", fixed = T)[[1]]
  params_types <- sapply(params_names, function(i){
    if(grepl("poly(",i,fixed=T)){
      return("numeric")
    }else if(class(model)=="lm"){
      return(class(model$model[[i]]))
    }else{
      return(class(model$data[[i]]))
    }

  })

  params_names <- c("Constante",params_names)
  params_types <- c("numeric", params_types)

  ## creation d'un beau tableau
  allrows <- lapply(1:length(params_names), function(i){
    pname <- params_names[[i]]
    ptype <- params_types[[i]]
    rn <- rownames(base_table)
    if(ptype %in% c("character","factor")){
      rows <- base_table[grepl(pname,x = rn,fixed = T),]
      if(class(model)[[1]]=="lm"){
        uvalues <- unique(as.character(model$model[[pname]]))
      }else{
        uvalues <- unique(as.character(model$data[[pname]]))
      }
      if(length(uvalues)>2){
        new_names <- gsub(pattern = pname, x = row.names(rows), replacement = "", fixed = T)
      }else{
        oldname <- rn[grepl(pname,x = rn,fixed = T)]
        new_names <- gsub(pattern = pname, x = oldname, replacement = "", fixed = T)
      }
      ref <- unique(uvalues[! uvalues %in% new_names])
      new_table <- rbind("--", rows)
      new_table <- cbind(c(paste("ref : ",ref,sep=""),new_names),new_table)
      row1 <- c(paste("*",pname,"*",sep=""),rep("",ncol(new_table)-1))
      new_table <- rbind(row1,new_table)
      return(new_table)
    }
    if(ptype %in% c("integer", "double", "numeric")){
      if(pname == "Constante"){
        row <- base_table[rn=="(Intercept)",]
      }else if(grepl("poly(",pname,fixed=T)){
        part <- strsplit(pname,",",fixed=T)[[1]][[1]]
        rows <- base_table[grepl(part,rn,fixed=T),]
        varn <- strsplit(part,"(",fixed=T)[[1]][[2]]
        rnames <- paste(varn," ordre ", 1:nrow(rows), sep = "")
        rows <- cbind(rnames, rows)
        return(rows)
      }else{
        row <- base_table[rn==pname,]
      }
      row <- c(pname, row)
      return(row)
    }
  })

  if(is.na(testt)){
    letter <- "z"
  }else{
    letter <- "t"
  }

  final_table <- do.call(rbind,allrows)
  if(class(model)!="lm"){
    if(model$family$link == "logit" & confid){
      colnames(final_table) <- c("variable", "coefficient", "OR",
                                 "err. std",paste("val.",letter), "val .p",
                                 "coeff 2.5%", "coeff 97.5%",
                                 "OR 2.5%", "oR 97.5%")
      final_table <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                  z_digits,p_digits,coef_digits,coef_digits,
                                                  OR_digits,OR_digits))

    }else if (model$family$link == "logit" & confid==F){
      colnames(final_table) <- c("variable", "coefficient", "OR",
                                 "err. std",paste("val.",letter), "val .p")
      final_table <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                  z_digits,p_digits))

    }else if(model$family$link == "log" & confid){
      colnames(final_table) <- c("variable", "coefficient", "exp(coeff)",
                                 "err. std",paste("val.",letter), "val .p",
                                 "coeff 2.5%", "coeff 97.5%",
                                 "exp(coeff 2.5%)", "exp(coeff 97.5%)")
      final_table <- clean_columns(final_table, c(NA, coef_digits, coef_digits, std_digits,
                                                  z_digits,p_digits,coef_digits,coef_digits,
                                                  coef_digits,coef_digits))

    }else if (model$family$link == "logit" & confid==F){
      colnames(final_table) <- c("variable", "coefficient", "exp(coeff)",
                                 "err. std",paste("val.",letter), "val .p")
      final_table <- clean_columns(final_table, c(NA, coef_digits, coef_digits, std_digits,
                                                  z_digits,p_digits))

    }else if(confid){
      colnames(final_table) <- c("variable", "coefficient",
                                 "err. std",paste("val.",letter), "val .p",
                                 "coeff 2.5%", "coeff 97.5%")
      final_table <- clean_columns(final_table, c(NA, coef_digits, std_digits,
                                                  z_digits,p_digits, coef_digits, coef_digits))

    }else if(confid==F){
      colnames(final_table) <- c("variable", "coefficient",
                                 "err. std",paste("val.",letter), "val .p")
      final_table <- clean_columns(final_table, c(NA, coef_digits, std_digits,
                                                  z_digits,p_digits))
    }

  }else{
    if(confid){
      colnames(final_table) <- c("variable", "coefficient",
                                 "err. std",paste("val.",letter), "val .p",
                                 "coeff 2.5%", "coeff 97.5%")
      final_table <- clean_columns(final_table, c(NA, coef_digits, std_digits,
                                                  z_digits,p_digits, coef_digits, coef_digits))
    }else if(confid==F){
      colnames(final_table) <- c("variable", "coefficient",
                                 "err. std",paste("val.",letter), "val .p")
      final_table <- clean_columns(final_table, c(NA, coef_digits, std_digits,
                                                  z_digits,p_digits))
    }

  }

  rownames(final_table) <- NULL
  return(final_table)
}

## pour un gam
build_table.gam <- function(model, confid = T, coef_digits = 2, std_digits = 2, z_digits = 2, p_digits = 3, OR_digits = 3){
  ## extraction des elements principaux
  base_table <- summary(model)
  coeffs <- model$coefficients
  test <- grepl("s(",names(coeffs),fixed=T) == F
  base_table <- cbind(coeffs[test],
                      base_table$se[test],
                      base_table$p.t[test],
                      base_table$p.pv[test]
                      )
  colnames(base_table) <- c("Estimate","Std. Error", "z value","Pr(>|z|)")

  ## calcule des intervale de confiance sur les coeffs
  if(confid){
    base_table <- cbind(base_table, round(hand_contint.gam(model),coef_digits))
  }

  ## si fonction de lien = logit : OR
  if (model$family$link == "logit"){
    base_table <- cbind(base_table[,1], round(exp(base_table[,1]),OR_digits) , base_table[,2:ncol(base_table)])
    if(confid){
      n <- ncol(base_table)
      base_table <- cbind(base_table, round(exp(base_table[,c(n-1,n)]), OR_digits))
    }
  }else if (model$family$link == "log"){
    base_table <- cbind(base_table[,1], round(exp(base_table[,1]),coef_digits) , base_table[,2:ncol(base_table)])
    if(confid){
      n <- ncol(base_table)
      base_table <- cbind(base_table, round(exp(base_table[,c(n-1,n)]), coef_digits))
    }
  }else if (as.character(model$family)[[1]] == "ziplss"){
    base_table <- cbind(base_table[,1], round(exp(base_table[,1]),coef_digits) , base_table[,2:ncol(base_table)])
    if(confid){
      n <- ncol(base_table)
      base_table <- cbind(base_table, round(exp(base_table[,c(n-1,n)]), coef_digits))
    }
  }

  ## gerer les arrondis
  base_table[,1] <- round(base_table[,1], coef_digits)
  i<-match("Std. Error", colnames(base_table))
  base_table[,i] <- round(base_table[,i], std_digits)
  i<-match("z value", colnames(base_table))
  base_table[,i] <- round(base_table[,i], z_digits)
  i<-match("Pr(>|z|)", colnames(base_table))
  base_table[,i] <- round(base_table[,i], p_digits)

  if(as.character(model$family)[[1]] %in% c("ziplss")){
    params_names <- strsplit(as.character(model$formula)[[1]]," ~ ", fixed = T)[[1]][[2]]
    params_names2 <- as.character(model$formula)[[2]]
    params_names <- strsplit(params_names," + ", fixed = T)[[1]]
    params_names2 <- strsplit(params_names2," + ", fixed = T)[[1]]
    params_names2 <- params_names2[2:length(params_names2)]
    params_names <- params_names[grepl("s(",params_names, fixed=T)==F]
    params_names2 <- params_names2[grepl("s(",params_names2, fixed=T)==F]
    params_types <- sapply(params_names, function(i){
      class(model$model[[i]])
    })
    params_types2 <- sapply(params_names2, function(i){
      class(model$model[[i]])
    })
    params_names <- c("Constante",params_names)
    params_types <- c("numeric", params_types)
    params_names2 <- c("Constante",params_names2)
    params_types2 <- c("numeric", params_types2)

  }else{
    params_names <- as.character(model$formula)
    params_names <- params_names[3:length(params_names)]
    params_names <- strsplit(params_names," + ", fixed = T)[[1]]
    params_names <- params_names[grepl("s(",params_names, fixed=T)==F]
    params_types <- sapply(params_names, function(i){
      class(model$model[[i]])
    })
    params_names2 <- NULL
    params_names <- c("Constante",params_names)
    params_types <- c("numeric", params_types)
  }

  # formatting the values
  base_table <- format(base_table, big.mark  = " ", decimal.mark = ",")

  ## creation d'un beau tableau
  allrows <- lapply(1:length(params_names), function(i){
    pname <- params_names[[i]]
    ptype <- params_types[[i]]
    rn <- rownames(base_table)
    if(ptype %in% c("character","factor")){
      rows <- base_table[grepl(pname,x = rn,fixed = T) & grepl(".1",x=rn,fixed=F)==F,]
      uvalues <- unique(as.character(model$data[[pname]]))
      if(length(uvalues)>2){
        new_names <- gsub(pattern = pname, x = row.names(rows), replacement = "", fixed = T)
      }else{
        oldname <- rn[grepl(pname,x = rn,fixed = T)]
        new_names <- gsub(pattern = pname, x = oldname, replacement = "", fixed = T)
      }
      ref <- unique(uvalues[! uvalues %in% new_names])
      new_table <- rbind("--", rows)
      new_table <- cbind(c(paste("ref : ",ref,sep=""),new_names),new_table)
      row1 <- c(paste("*",pname,"*",sep=""),rep("",ncol(new_table)-1))
      new_table <- rbind(row1,new_table)
      return(new_table)
    }
    if(ptype %in% c("integer", "double", "numeric")){
      if(pname == "Constante"){
        row <- base_table[rn=="(Intercept)",]
      }else{
        row <- base_table[rn==pname,]
      }
      row <- c(pname, row)
      return(row)
    }
  })

  final_table <- do.call(rbind,allrows)

  if(is.null(params_names2)==F){
    allrows2 <- lapply(1:length(params_names2), function(i){
      pname <- params_names2[[i]]
      ptype <- params_types2[[i]]
      rn <- rownames(base_table)
      if(ptype %in% c("character","factor")){
        rows <- base_table[grepl(pname,x = rn,fixed = T) & grepl(".1",x=rn,fixed=F)==T,]
        uvalues <- unique(as.character(model$data[[pname]]))
        if(length(uvalues)>2){
          new_names <- gsub(pattern = pname, x = row.names(rows), replacement = "", fixed = T)
        }else{
          oldname <- rn[grepl(pname,x = rn,fixed = T)]
          new_names <- gsub(pattern = pname, x = oldname, replacement = "", fixed = T)
        }
        ref <- unique(uvalues[! uvalues %in% new_names])
        new_table <- rbind("--", rows)
        new_table <- cbind(c(paste("ref : ",ref,sep=""),new_names),new_table)
        row1 <- c(paste("*",pname,"*",sep=""),rep("",ncol(new_table)-1))
        new_table <- rbind(row1,new_table)
        return(new_table)
      }
      if(ptype %in% c("integer", "double", "numeric")){
        if(pname == "Constante"){
          row <- base_table[rn=="(Intercept).1",]
        }else{
          row <- base_table[rn==paste(pname,".1",sep=""),]
        }
        row <- c(pname, row)
        return(row)
      }
    })

    final_table2 <- do.call(rbind,allrows2)
    if(confid){
      colnames(final_table2) <- c("variable", "coefficient", "OR",
                                  "err. std","val. z", "val .p",
                                  "coeff 2.5%", "coeff 97.5%",
                                  "OR 2.5%", "OR 97.5%")
      final_table <- final_table[,c(1,2,3,4,5,6,7,8,9,10)]

      final_table <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                  z_digits,p_digits, coef_digits, coef_digits,
                                                  OR_digits, OR_digits))

      colnames(final_table) <- c("variable", "coefficient", "exp(coeff)",
                                 "err. std","val. z", "val .p",
                                 "coeff 2.5%", "coeff 97.5%",
                                 "exp(coeff) 2.5%", "exp(coeff) 97.5%")
      final_table2 <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                  z_digits,p_digits, coef_digits, coef_digits,
                                                  OR_digits, OR_digits))
    }else{
      colnames(final_table2) <- c("variable", "coefficient", "OR",
                                  "err. std","val. z", "val .p")
      final_table <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                  z_digits,p_digits))
      final_table <- final_table[,c(1,2,3,4,5,6)]
      colnames(final_table) <- c("variable", "coefficient", "exp(coeff)",
                                 "err. std","val. z", "val .p")
      final_table2 <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                   z_digits,p_digits))

    }
    rownames(final_table2) <- NULL
    rownames(final_table) <- NULL
    return(list(final_table, final_table2))
  }


  if(model$family$link == "logit" & confid){
    colnames(final_table) <- c("variable", "coefficient", "OR",
                               "err. std","val. z", "val .p",
                               "coeff 2.5%", "coeff 97.5%",
                               "OR 2.5%", "oR 97.5%")
    final_table <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                z_digits,p_digits, coef_digits, coef_digits,
                                                OR_digits, OR_digits))

  }else if (model$family$link == "logit" & confid==F){
    colnames(final_table) <- c("variable", "coefficient", "OR",
                               "err. std","val. z", "val .p")
    final_table <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                z_digits,p_digits))

  }else if(model$family$link == "log" & confid){
    colnames(final_table) <- c("variable", "coefficient", "exp(coeff)",
                               "err. std","val. z", "val .p",
                               "coeff 2.5%", "coeff 97.5%",
                               "exp(coeff) 2.5%", "exp(coeff) 97.5%")
    final_table <- clean_columns(final_table, c(NA, coef_digits, coef_digits, std_digits,
                                                z_digits,p_digits, coef_digits, coef_digits,
                                                coef_digits, coef_digits))

  }else if (model$family$link == "log" & confid==F){
    colnames(final_table) <- c("variable", "coefficient", "exp(coeff)",
                               "err. std","val. z", "val .p")
    final_table <- clean_columns(final_table, c(NA, coef_digits, coef_digits, std_digits,
                                                z_digits,p_digits))

  }else if(confid){
    colnames(final_table) <- c("variable", "coefficient",
                               "err. std","val. z", "val .p",
                               "coeff 2.5%", "coeff 97.5%")
    final_table <- clean_columns(final_table, c(NA, coef_digits, std_digits,
                                                z_digits,p_digits,coef_digits,coef_digits))

  }else if(confid==F){
    colnames(final_table) <- c("variable", "coefficient",
                               "err. std","val. z", "val .p")
    final_table <- clean_columns(final_table, c(NA, coef_digits, std_digits,
                                                z_digits,p_digits))
  }
  rownames(final_table) <- NULL

  return(final_table)

}

# confid = T
# coef_digits = 2
# std_digits = 2
# z_digits = 2
# p_digits = 3
# OR_digits = 3

## pour un vglm
build_table.vglm <- function(model, confid = T, coef_digits = 2, std_digits = 2, z_digits = 2, p_digits = 3, OR_digits = 3){
  ## extraction des elements principaux
  base_table <- summary(model)@coef3

  ## calcule des intervale de confiance sur les coeffs
  if(confid){
    base_table <- cbind(base_table, round(confint(model),coef_digits))
  }

  ## si fonction de lien = logit : OR
  if (model@family@vfamily[[1]] %in% c("cumulative", "binomial", "multinomial")){
    base_table <- cbind(base_table[,1], round(exp(base_table[,1]),OR_digits) , base_table[,2:ncol(base_table)])
    if(confid){
      n <- ncol(base_table)
      base_table <- cbind(base_table, round(exp(base_table[,c(n-1,n)]), OR_digits))
    }
  }else if (model@family@vfamily[[1]] %in% c("gamma2")){
    base_table <- cbind(base_table[,1],
                        round(exp(base_table[,1]),OR_digits),
                        base_table[,c(2,3,4)],
                        round(exp(base_table[,c(5,6)]),OR_digits)
                        )
  }

  ## gerer les arrondis
  base_table[,1] <- round(base_table[,1], coef_digits)
  i<-match("Std. Error", colnames(base_table))
  base_table[,i] <- round(base_table[,i], std_digits)
  i<-match("z value", colnames(base_table))
  base_table[,i] <- round(base_table[,i], z_digits)
  i<-match("Pr(>|z|)", colnames(base_table))
  base_table[,i] <- round(base_table[,i], p_digits)

  # formatting the values
  base_table <- format(base_table, big.mark  = " ", decimal.mark = ",")

  params_names <- strsplit(as.character(model@terms)," ~ ")[[1]][[2]]
  params_names <- strsplit(params_names," + ", fixed = T)[[1]]
  params_types <- sapply(params_names, function(i){
    class(model@model[[i]])
  })


  if(model@family@vfamily[[1]] == "cumulative"){
    inter_names <- rownames(base_table)[grepl("(Intercept",rownames(base_table),fixed = T)]
    params_names <- c(inter_names, params_names)
    params_types <- c(rep("numeric", length(inter_names)),params_types)

    ##NB : dealing with not parallel elements
    if (isTRUE(model@family@infos()$parallel) == FALSE){
      elements <- as.character(model@family@infos()$parallel[[3]])
      if ("+" %in% elements){
        not_paralelle <- elements[2:length(elements)]
      }else{
        not_paralelle <- elements
      }

      test <- (params_names %in%  not_paralelle) == F
      params_names <- params_names[test]
      params_types <- params_types[test]
    }else{
      not_paralelle <- c()
    }


  }else{
    params_names <- c("Constante",params_names)
    params_types <- c("numeric", params_types)
  }

  ## creation d'un beau tableau
  if(model@family@vfamily[[1]] == "gamma2"){
    rownames(base_table)[1] <- "(Intercept)"
    rownames(base_table)[2] <- "shape"
  }

  if(model@family@vfamily == "multinomial"){

    final_table <- basetable.multinom(model,base_table,params_names,params_types)

  }else{
    rn <- rownames(base_table)
    allrows <- lapply(1:length(params_names), function(i){
      pname <- params_names[[i]]
      ptype <- params_types[[i]]
      if(ptype %in% c("character","factor")){
        rows <- base_table[grepl(pname,x = rn,fixed = T),]
        uvalues <- unique(as.character(model@model[[pname]]))
        if(length(uvalues)>2){
          new_names <- gsub(pattern = pname, x = row.names(rows), replacement = "", fixed = T)
        }else{
          oldname <- rn[grepl(pname,x = rn,fixed = T)]
          new_names <- gsub(pattern = pname, x = oldname, replacement = "", fixed = T)
        }
        ref <- unique(uvalues[! uvalues %in% new_names])
        new_table <- rbind("--", rows)
        new_table <- cbind(c(paste("ref : ",ref,sep=""),new_names),new_table)
        row1 <- c(paste("*",pname,"*",sep=""),rep("",ncol(new_table)-1))
        new_table <- rbind(row1,new_table)
        return(new_table)
      }
      if(ptype %in% c("integer", "double", "numeric")){
        if(pname == "Constante"){
          row <- base_table[rn=="(Intercept)",]
        }else{
          row <- base_table[rn==pname,]
        }
        row <- c(pname, row)
        return(row)
      }
    })

    ## ajouter les lignes pour la fonction cumulative
    if(model@family@vfamily[[1]] == "cumulative"){
      k <- 1
      more_rows <- lapply(not_paralelle,function(i){
        type_param <- class(model@model[[i]])
        if(type_param %in% c("character","factor")){
          rows <- base_table[grepl(i,x = rn,fixed = T),]
          uvalues <- unique(as.character(model@model[[i]]))
          new_names <- gsub(pattern = i, x = row.names(rows), replacement = "", fixed = T)
          new_names2 <- unique(sapply(new_names, function(j){strsplit(j,":",fixed=T)[[1]][[1]]}))
          ref <- unique(uvalues[! uvalues %in% new_names2])
          new_table <- rbind("--", rows)
          new_table <- cbind(c(paste("ref : ",ref,sep=""),new_names),new_table)
          row1 <- c(paste("*",i,"*",sep=""),rep("",ncol(new_table)-1))
          new_table <- rbind(row1,new_table)
        }

        if(type_param %in% c("integer", "double", "numeric")){
          rows <- base_table[grepl(i,x = rn,fixed = T),]
          rnames <- paste(paste(rep(i),":",sep=""),1:(ncol(model@y)-1), sep="")
          new_table <- cbind(rnames, rows)
        }
        if(k == 1){
          new_table <- rbind(rep("",ncol(new_table)),c("**Effets par niveau**", rep("",ncol(new_table)-1)), new_table)
        }

        k <<- k + 1
        return(new_table)
      })

      allrows <- append(allrows,more_rows)
    }

    ## ajouter les lignes pour le modele de gamma2
    if(model@family@vfamily[[1]] == "gamma2"){
      row2 <- base_table[grepl("shape",x = rn,fixed = T),]
      row2 <- c("shape",row2)
      row1 <- rep("",length(row2))
      more_rows <- list(rbind(row1,row2))
      allrows <- append(allrows,more_rows)
    }

    final_table <- do.call(rbind,allrows)
  }



  if(model@family@vfamily[[1]] %in% c("cumulative", "binomial") & confid){
    colnames(final_table) <- c("variable", "coefficient", "OR",
                               "err. std","val. z", "val .p",
                               "coeff 2.5%", "coeff 97.5%",
                               "OR 2.5%", "oR 97.5%")

    final_table <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                z_digits,p_digits, coef_digits, coef_digits,
                                                OR_digits, OR_digits))

  }else if (model@family@vfamily[[1]] %in% c("cumulative", "binomial") & confid==F){
    colnames(final_table) <- c("variable", "coefficient", "OR",
                               "err. std","val. z", "val .p")

    final_table <- clean_columns(final_table, c(NA, coef_digits, OR_digits, std_digits,
                                                z_digits,p_digits))

  }else if (model@family@vfamily[[1]] == "multinomial"){
    oldnames <- names(final_table)
    final_table2 <- lapply(final_table,function(el){
      if(confid){
        colnames(el) <- c("variable", "coefficient", "OR",
                          "err. std","val. z", "val .p",
                          "coeff 2.5%", "coeff 97.5%",
                          "OR 2.5%", "oR 97.5%")
        el <-  clean_columns(el, c(NA, coef_digits, OR_digits, std_digits,
                                   z_digits,p_digits, coef_digits, coef_digits,
                                   OR_digits, OR_digits))
      }else{
        colnames(el) <- c("variable", "coefficient", "OR",
                          "err. std","val. z", "val .p")
        el <-  clean_columns(el, c(NA, coef_digits, OR_digits, std_digits,
                                            z_digits,p_digits))
      }
      return(el)
    })
    names(final_table2) <- oldnames
    final_table <- final_table2

  }else if (model@family@vfamily[[1]] %in% c("gamma2") & confid==F){
    colnames(final_table) <- c("variable", "coefficient", "exp(coefficient)",
                               "err. std","val. z", "val .p")

    final_table <- clean_columns(final_table, c(NA, coef_digits, coef_digits, std_digits,
                                                z_digits,p_digits))

  }else if (model@family@vfamily[[1]] %in% c("gamma2") & confid==T){
    colnames(final_table) <- c("variable", "coefficient", "exp(coefficient)",
                               "err. std","val. z", "val .p","coeff 2.5%", "coeff 97.5%")
    final_table <- clean_columns(final_table, c(NA, coef_digits, coef_digits, std_digits,
                                                z_digits,p_digits,coef_digits,coef_digits))

  }else if(confid){
    colnames(final_table) <- c("variable", "coefficient",
                               "err. std","val. z", "val .p",
                               "coeff 2.5%", "coeff 97.5%")
    final_table <- clean_columns(final_table, c(NA, coef_digits, std_digits,
                                                z_digits,p_digits,coef_digits,coef_digits))
  }else if(confid==F){
    colnames(final_table) <- c("variable", "coefficient",
                               "err. std","val. z", "val .p")
    final_table <- clean_columns(final_table, c(NA, coef_digits, std_digits,
                                                z_digits,p_digits))
  }
  rownames(final_table) <- NULL
  return(final_table)
}


basetable.multinom <- function(model,base_table,params_names,params_types){
  ## checker les parametres generaux
  fixedformula <- model@family@infos()$parallel
  if(fixedformula != FALSE){
    terms <- as.character(fixedformula[[3]])
    terms <- terms[terms!="+"]
    if("1" %in% terms){
      terms[terms == "1"] <- "Constante"
    }
    test <- params_names %in% terms
    terms_types <- params_types[match(terms,params_names)]
    terms_types <- ifelse(is.na(terms_types), "numeric",terms_types)
    params_names <- params_names[test == F]
    params_types <- params_types[test == F]
  }

  rn <- rownames(base_table)
  types <- model@extra$colnames.y
  reflevel <- types[model@extra$use.refLevel]
  complevels <- types[types!=reflevel]
  ids <- 1:length(complevels)

  ## tableau pour tous les parametres classiques
  list_rows <- lapply(ids, function(j){
    idpart <- paste(":",j,sep="")
    allrows <- lapply(1:length(params_names), function(i){
      pname <- params_names[[i]]
      ptype <- params_types[[i]]
      if(ptype %in% c("character","factor")){
        rows <- base_table[grepl(pname,x = rn,fixed = T) & grepl(idpart,x = rn,fixed = T),]
        uvalues <- unique(as.character(model@model[[pname]]))
        if(length(uvalues)>2){
          new_names <- gsub(pattern = pname, x = row.names(rows), replacement = "", fixed = T)
          new_names <- gsub(pattern = idpart, x = new_names, replacement = "", fixed = T)
        }else{
          oldname <- rn[grepl(pname,x = rn,fixed = T)]
          new_names <- gsub(pattern = pname, x = oldname, replacement = "", fixed = T)
          new_names <- gsub(pattern = idpart, x = new_names, replacement = "", fixed = T)
        }
        ref <- unique(uvalues[! uvalues %in% new_names])
        new_table <- rbind("--", rows)
        new_table <- cbind(c(paste("ref : ",ref,sep=""),new_names),new_table)
        row1 <- c(paste("*",pname,"*",sep=""),rep("",ncol(new_table)-1))
        new_table <- rbind(row1,new_table)
        return(new_table)
      }
      if(ptype %in% c("integer", "double", "numeric")){
        if(pname == "Constante"){
          row <- base_table[rn==paste("(Intercept)",idpart,sep=""),]
        }else{
          row <- base_table[rn==paste(pname,idpart,sep=""),]
        }
        row <- c(pname, row)
        return(row)
      }
    })
  return(do.call(rbind,allrows))
  })

  names(list_rows) <- paste(reflevel," VS ",complevels, sep="")

  ## tableau pour tous les parametres fixes
  if(fixedformula != FALSE){
    allrows_fixed <- lapply(1:length(terms), function(i){
      pname <- terms[[i]]
      ptype <- terms_types[[i]]
      if(ptype %in% c("character","factor")){
        rows <- base_table[grepl(pname,x = rn,fixed = T),]
        uvalues <- unique(as.character(model@model[[pname]]))
        if(length(uvalues)>2){
          new_names <- gsub(pattern = pname, x = row.names(rows), replacement = "", fixed = T)
        }else{
          oldname <- rn[grepl(pname,x = rn,fixed = T)]
          new_names <- gsub(pattern = pname, x = oldname, replacement = "", fixed = T)
        }
        ref <- unique(uvalues[! uvalues %in% new_names])
        new_table <- rbind("--", rows)
        new_table <- cbind(c(paste("ref : ",ref,sep=""),new_names),new_table)
        row1 <- c(paste("*",pname,"*",sep=""),rep("",ncol(new_table)-1))
        new_table <- rbind(row1,new_table)
        return(new_table)
      }
      if(ptype %in% c("integer", "double", "numeric")){
        if(pname == "Constante"){
          row <- base_table[rn=="(Intercept)",]
        }else{
          row <- base_table[rn==pname,]
        }
        row <- c(pname, row)
        return(row)
      }
    })
  fixed_table <- do.call(rbind,allrows_fixed)
  onames <- names(list_rows)
  list_rows <- append(list_rows,list(fixed_table))
  names(list_rows) <- c(onames,"Parametre Fixes")
  }



  return(list_rows)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### calculer des intervals de confiance ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hand_contint.gam <- function(model){
  base_table <- summary(model)
  coeffs <- model$coefficients
  test <- grepl("s(",names(coeffs),fixed=T) == F
  ok_coeff <- coeffs[test]
  ok_se <- base_table$se[test]
  freed <- length(model$y) -model$rank
  mat <- cbind(
    (ok_coeff - qt(0.975, df = freed) * ok_se),
    (ok_coeff + qt(0.975, df = freed) * ok_se)
  )
  colnames(mat) <- c("2.5%","97.5%")
  return(mat)

}


data(iris)
model <- lm(Sepal.Length ~ poly(Sepal.Width,degree = 2) + Petal.Length + Species, data = iris)
build_table(model)
