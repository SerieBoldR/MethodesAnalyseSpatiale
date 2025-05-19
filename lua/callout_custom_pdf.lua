function Div(el)
  -- Vérification et création des tcolorbox pour chaque type de callout avec images
  local tcolorbox_open = '\\begin{tcolorbox}[colback=%s, colframe=%s, boxrule=0.2mm, leftrule=1mm, coltitle=black, fonttitle=\\bfseries, title={\\includegraphics[width=0.3cm]{%s} \\textbf{%s}}]'
  local tcolorbox_close = '\\end{tcolorbox}'

  if el.classes:includes("bloc_package") then
    table.insert(el.content, 1, pandoc.RawBlock('latex', tcolorbox_open:format('background_color', 'package_color', 'images/BlocPackage.png', 'Package')))
    table.insert(el.content, pandoc.RawBlock('latex', tcolorbox_close))
  elseif el.classes:includes("bloc_notes") then
    table.insert(el.content, 1, pandoc.RawBlock('latex', tcolorbox_open:format('background_color', 'notes_color', 'images/BlocNote.png','Note')))
    table.insert(el.content, pandoc.RawBlock('latex', tcolorbox_close))
  elseif el.classes:includes("bloc_aller_loin") then
    table.insert(el.content, 1, pandoc.RawBlock('latex', tcolorbox_open:format('background_color', 'allerloin_color', 'images/BlocAllerPlusLoin.png','Aller plus loin')))
    table.insert(el.content, pandoc.RawBlock('latex', tcolorbox_close))
  elseif el.classes:includes("bloc_astuce") then
    table.insert(el.content, 1, pandoc.RawBlock('latex', tcolorbox_open:format('background_color', 'astuce_color', 'images/BlocAstuce.png','Astuce')))
    table.insert(el.content, pandoc.RawBlock('latex', tcolorbox_close))
  elseif el.classes:includes("bloc_attention") then
    table.insert(el.content, 1, pandoc.RawBlock('latex', tcolorbox_open:format('background_color', 'attention_color', 'images/BlocAttention.png','Attention')))
    table.insert(el.content, pandoc.RawBlock('latex', tcolorbox_close))
  elseif el.classes:includes("bloc_objectif") then
    table.insert(el.content, 1, pandoc.RawBlock('latex', tcolorbox_open:format('background_color', 'objectif_color', 'images/BlocObjectif.png','Objectif')))
    table.insert(el.content, pandoc.RawBlock('latex', tcolorbox_close))
  elseif el.classes:includes("bloc_exercice") then
    table.insert(el.content, 1, pandoc.RawBlock('latex', tcolorbox_open:format('background_color', 'exercise_color', 'images/BlocExercice.png','Exercice')))
    table.insert(el.content, pandoc.RawBlock('latex', tcolorbox_close))
  end

  return el
end
