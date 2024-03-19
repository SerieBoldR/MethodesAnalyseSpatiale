library(htmltools)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Objet de base question ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# label : question a poser
# answers : les reponses possibles
# type : le type de question :
#   uc, unique choice
#   mc, multiple choice
#   stat, reponse statique
# response :
#   uc et mc : index des bonnes reponses
#   stat : string avec la bonne reponse
question <- function(label, type, response, answers = NULL, section = NULL){
  return(
    list("label" = label,
         "answers" = answers,
         "type" = type,
         "response" = response,
         "help" = help
    )
  )
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### Objet de base quizz ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

quizz <- function(yml_file, quizz_id){

  questions <- yaml::read_yaml(yml_file, fileEncoding = "UTF-8")

  return(
    list("questions" = questions,
         "id" = quizz_id)
  )
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### build html response ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_html_response <- function(quest, i){

  b1 <- tags$div(class = "quizlib-question-answers")

  ## pour une question de type uc
  if(quest$type == "uc"){
    j <- 1
    b2 <- tags$table(class='table table-sm table-striped small')
    for (ans in quest$answers){
      el <- tags$tr(
        tags$td(tags$label(
          ans)),
        tags$td(
          tags$input(
            type = "radio",
            name = paste0("q",i),
            value = j)
        ))
      b2$children[[j]] <- el
      j <- j+1
    }
  }

  ## pour une question de type mc
  if(quest$type == "mc"){
    j <- 1
    b2 <- tags$table(class='table table-sm table-striped small')
    for (ans in quest$answers){
      el <- tags$tr(
        tags$td(tags$label(
          ans)),
        tags$td(
          tags$input(
            type = "checkbox",
            name = paste0("q",i),
            value = j)
        ))
      b2$children[[j]] <- el
      j <- j+1
    }
  }

  ## pour une question de type stat
  if(quest$type == "stat"){
    b2 <- tags$input(type = "text", name = paste0("q",i))
  }
  b1$children[[1]] <- b2

  return(b1)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### build pdf response ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_pdf_responses <- function(myquizz){

  string <- ""
  for(quest in myquizz$questions){
    string <- paste0(string,"\n * ",quest$label)
    if(quest$type == "uc"){
      string <- paste0(string, "\n\t+ ", quest$answers[[quest$response]])
    }
    if(quest$type == "stat"){
      string <- paste0(string, "\n\t+ ", quest$response)
    }
    if(quest$type == "mc"){
      s2 <- paste0("\n\t+ ",paste(quest$answers[quest$response],collapse="\n\t+ "))
      string <- paste0(string, s2)
    }
  }


  return(string)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### quizz to PDF ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render_quizz_pdf <- function(myquizz){
  string <- ""

  for (quest in myquizz$questions){

    string <- paste0(string, "\n* ","**",quest$label,"**")

    if(is.null(quest$image) == FALSE){
      string <- paste0(string, "\n\n\t ![",quest$alt_txt,"](",quest$image,"){width=50%}", "\n")
    }

    if(quest$type == "stat"){
      string <- paste0(string, "\n\t+ ","...")
    }else{
      s2 <- paste0("\n\t+ ",paste(quest$answers,collapse="\n\t+ "))
      string <- paste0(string, s2)
    }
    if(is.null(quest$help) == FALSE){
      string <- paste0(string, "\n\n\t",quest$help, "\n")
    }

  }

  questions <- string
  responses <- build_pdf_responses(myquizz)

  el2 <- "**Réponses**\n"
  Encoding(el2) <- "UTF-8"

  final_string <- paste0(
    "**Questions**\n",
    questions,"\n\n",
    el2,
    responses
  )
  return(final_string)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### quizz to html ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_html_quizz <- function(myquizz){

  # creation de la balise centrale pour le quizz
  quizz_div <- list()

  # creation des questions dans le quizz
  i <- 1

  for(quest in myquizz$questions){
    balise <- tags$div(class = "card quizlib-question")
    # creation de la balise question
    b1 <- tags$div(quest$label,class = "quizlib-question-title")
    # creation de la balise image
    if(is.null(quest$image) == FALSE){
      bimg <- tags$img(src=quest$image,
                       alt=quest$alt_txt,
                       style = "width: 50%;display: block;margin-left: auto;margin-right: auto;margin-bottom: 0.5em;margin-top: 0.5em;")
    }
    # creation de la balise reponses
    resp <- build_html_response(quest, i)

    # ajouter l'astuce
    if(is.null(quest$help) == FALSE){
      div_sec <- tags$div(
        #paste0("voir section : \\@ref(",quest$section,")")
        quest$help
      )
    }

    childs <- list(b1)

    if(is.null(quest$image) == FALSE){
      childs[[length(childs)+1]] <- bimg
    }

    if(is.null(quest$help) == FALSE){
      childs[[length(childs)+1]] <- div_sec
    }

    childs[[length(childs)+1]] <- resp
    balise$children <- childs

    quizz_div[[i]] <- balise
    i <- i+1
  }

  # ajouter le bouton de verification
  on_click <- paste0("showResults(quizz_",myquizz$id,");")
  result <- tags$button("Vérifier votre résultat", type="button", onclick=on_click, id="buttonID")

  quizz_div[[length(quizz_div)+1]] <- result

  # ajout d'une balise de resultat
  res_div <- tags$div(tags$span(id = "quiz-percent"),id="quiz-result", class="card")

  quizz_div[[length(quizz_div)+1]] <- res_div

  return(quizz_div)
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### javascript code to launch the quizz ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
js_code1 <- 'var Quiz=function(a,b){this.Classes=Object.freeze({QUESTION:"quizlib-question",QUESTION_TITLE:"quizlib-question-title",QUESTION_ANSWERS:"quizlib-question-answers",QUESTION_WARNING:"quizlib-question-warning",CORRECT:"quizlib-correct",INCORRECT:"quizlib-incorrect",TEMP:"quizlib-temp"}),this.unansweredQuestionText="Question sans réponse !",this.container=document.getElementById(a),this.questions=[],this.result=new QuizResult,this.answers=b;for(var c=0;c<this.container.children.length;c++)this.container.children[c].classList.contains(Quiz.Classes.QUESTION)&&this.questions.push(this.container.children[c]);if(this.answers.length!=this.questions.length)throw new Error("Number of answers does not match number of questions!")};Quiz.Classes=Object.freeze({QUESTION:"quizlib-question",QUESTION_TITLE:"quizlib-question-title",QUESTION_ANSWERS:"quizlib-question-answers",QUESTION_WARNING:"quizlib-question-warning",CORRECT:"quizlib-correct",INCORRECT:"quizlib-incorrect",TEMP:"quizlib-temp"}),Quiz.prototype.checkAnswers=function(a){void 0===a&&(a=!0);for(var b=[],c=[],d=0;d<this.questions.length;d++){var e=this.questions[d],f=this.answers[d],g=[];this.clearHighlights(e);for(var h,i=e.getElementsByClassName(Quiz.Classes.QUESTION_ANSWERS)[0].getElementsByTagName("input"),j=0;j<i.length;j++)h=i[j],"checkbox"===h.type||"radio"===h.type?h.checked&&g.push(h.value):""!==h.value&&g.push(h.value);1!=g.length||Array.isArray(f)?0===g.length&&b.push(e):g=g[0],c.push(Utils.compare(g,f))}if(0===b.length||!a)return this.result.setResults(c),!0;for(d=0;d<b.length;d++){var k=document.createElement("span");k.appendChild(document.createTextNode(this.unansweredQuestionText)),k.className=Quiz.Classes.QUESTION_WARNING,b[d].getElementsByClassName(Quiz.Classes.QUESTION_TITLE)[0].appendChild(k)}return!1},Quiz.prototype.clearHighlights=function(a){for(var b=a.getElementsByClassName(Quiz.Classes.QUESTION_WARNING);b.length>0;)b[0].parentNode.removeChild(b[0]);var c,d=[a.getElementsByClassName(Quiz.Classes.CORRECT),a.getElementsByClassName(this.Classes.INCORRECT)];for(i=0;i<d.length;i++)for(;d[i].length>0;)c=d[i][0],c.classList.remove(Quiz.Classes.CORRECT),c.classList.remove(Quiz.Classes.INCORRECT);for(var e=a.getElementsByClassName(Quiz.Classes.TEMP);e.length>0;)e[0].parentNode.removeChild(e[0])},Quiz.prototype.highlightResults=function(a){for(var b,c=0;c<this.questions.length;c++)b=this.questions[c],b.getElementsByClassName(Quiz.Classes.QUESTION_TITLE)[0].classList.add(this.result.results[c]?Quiz.Classes.CORRECT:Quiz.Classes.INCORRECT),void 0!==a&&a(this,b,c,this.result.results[c])};var QuizResult=function(){this.results=[],this.totalQuestions=0,this.score=0,this.scorePercent=0,this.scorePercentFormatted=0};QuizResult.prototype.setResults=function(a){this.results=a,this.totalQuestions=this.results.length,this.score=0;for(var b=0;b<this.results.length;b++)this.results[b]&&this.score++;this.scorePercent=this.score/this.totalQuestions,this.scorePercentFormatted=Math.floor(100*this.scorePercent)};var Utils=function(){};Utils.compare=function(a,b){if(a.length!=b.length)return!1;if(Array.isArray(a)&&Array.isArray(b)){for(var c=0;c<a.length;c++)if(a[c]!==b[c])return!1;return!0}return a===b};'
js_code <- "
function showResults(quiz) {
    // Check answers and continue if all questions have been answered
    if (quiz.checkAnswers()) {
        var quizScorePercent = quiz.result.scorePercentFormatted; // The unformatted percentage is a decimal in range 0 - 1
        var quizResultElement = document.getElementById('quiz-result');
        quizResultElement.style.display = 'block';
        document.getElementById('quiz-percent').innerHTML = quizScorePercent.toString();

        // Change background colour of results div according to score percent
        if (quizScorePercent > 75) quizResultElement.style.backgroundColor = '#4caf50';
        else if (quizScorePercent > 50) quizResultElement.style.backgroundColor = '#ffc107';
        else if (quizScorePercent > 25) quizResultElement.style.backgroundColor = '#ff9800';
        else if (quizScorePercent > 0) quizResultElement.style.backgroundColor = '#f44336';

        // Highlight questions according to whether they were correctly answered. The callback allows us to highlight/show the correct answer
        quiz.highlightResults(handleAnswers);
    }
}

/** Callback for Quiz.highlightResults. Highlights the correct answers of incorrectly answered questions
 * Parameters are: the quiz object, the question element, question number, correctly answered flag
 */
function handleAnswers(quiz, question, no, correct) {
    if (!correct) {
        var answers = question.getElementsByTagName('input');
        for (var i = 0; i < answers.length; i++) {
            if (answers[i].type === 'checkbox' || answers[i].type === 'radio'){
                // If the current input element is part of the correct answer, highlight it
                if (quiz.answers[no].indexOf(answers[i].value) > -1) {
                    answers[i].parentNode.classList.add(Quiz.Classes.CORRECT);
                }
            } else {
                // If the input is anything other than a checkbox or radio button, show the correct answer next to the element
                var correctAnswer = document.createElement('span');
                correctAnswer.classList.add(Quiz.Classes.CORRECT);
                correctAnswer.classList.add(Quiz.Classes.TEMP); // quiz.checkAnswers will automatically remove elements with the temp class
                correctAnswer.innerHTML = quiz.answers[no];
                correctAnswer.style.marginLeft = '10px';
                answers[i].parentNode.insertBefore(correctAnswer, answers[i].nextSibling);
            }
        }
    }
}"

prep_jscode <- function(myquizz){
  id <- myquizz$id

  # preparer le code pour les reponses
  rep_string <- ""
  for (quest in myquizz$questions){
    if(quest$type == "uc"){
      rep_string <- paste0(rep_string,"'",quest$response,"',")
    }
    if(quest$type == "stat"){
      rep_string <- paste0(rep_string,"'",quest$response,"',")
    }
    if(quest$type == "mc"){
      string2 <- paste0("['",paste(quest$response, collapse = "', '"),"']")

      rep_string <- paste0(rep_string,string2,",")
    }
  }
  rep_string <- substr(rep_string, 1, nchar(rep_string)-1)
  rep_string <- paste0("[",rep_string,"]")

  total_code <- paste0("
  var quizz_",id,";
  window.onload = function() {
    quizz_",id," = new Quiz('",id,"', ",rep_string,");
  };")

  final_code <- paste(js_code1, js_code, total_code, sep = "\n")

}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### render the html quizz ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render_quizz_html<-function(myquizz){

  ## generating the base html
  html_code <- build_html_quizz(myquizz)

  ## linking the javascript code
  tag1 <- tags$link(type="text/javascript", src="libs/quizlib.1.0.1.min.js")
  tag3 <- tags$link(rel="stylesheet", type="text/css", src="css/quizlib.min.css")
  js_code <- prep_jscode(myquizz)
  tag2 <- tags$script(js_code)

  global_div <- tags$div(id = myquizz$id, class = "card")
  global_div$children <- c(list(tag1, tag2, tag3), html_code)

  #string <- doRenderTags(global_div)
  string <- doRenderTags(global_div, indent = FALSE)
  string <- as.character(string)
  string <- gsub(pattern = "&gt;", replacement = ">",string, fixed = TRUE)
  string <- gsub(pattern = "&lt;", replacement = "<",string, fixed = TRUE)
  string <- gsub(pattern = "&amp;&amp;", replacement = "&&",string, fixed = TRUE)
  return(string)

}



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#### final wrapper function ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

render_quizz <- function(myquizz){
  if (knitr::is_latex_output()){
    return(cat(render_quizz_pdf(myquizz)))
  }else{
    return(cat(render_quizz_html(myquizz)))
  }
}

