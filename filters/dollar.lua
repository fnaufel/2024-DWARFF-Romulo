-- Pandoc filter that replaces \(...\) with $...$ in latex output.
-- Other output formats are unaffected.
-- The hl.lua filter only works if this filter runs first.
function Math(el)
  
  if FORMAT:match 'latex' and el.mathtype == "InlineMath" then
    
    contents = el.text
    return pandoc.RawInline('latex', '$' .. contents .. '$')
    
  else
    return el
  end
  
end
