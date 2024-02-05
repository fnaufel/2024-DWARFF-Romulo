-- Pandoc filter that generate highlights in latex.  
-- Uses soul package.
-- Other output formats are unaffected.
-- See https://github.com/jgm/pandoc/issues/5529
-- The soul package can't handle some things inside code blocks.
-- This function wraps code blocks in \mbox{}
function Span(el)
  
  if FORMAT:match 'latex' and el.classes:includes('hl') then
    
    -- If there are Code elements within the span, wrap them in mbox{}
    limpo = el:walk {
      Code = function(el) 
        return pandoc.Span(
          pandoc.Inlines({
            pandoc.RawInline('latex', '\\mbox{'), 
            el,
            pandoc.RawInline('latex', '}'), 
          })
        )
      end
    }
    
    -- Apply \hl command to span
    return pandoc.Span(
      pandoc.Inlines({
        pandoc.RawInline('latex', '\\hl'), 
        limpo
      })
    )
    
  else
    return el
  end
  
end

