# My Pandoc filters

Resources:

* *Pandoc filters* <http://pandoc.org/filters.html>
* *Definition of Pandoc data structure for format-neutral representation of documents* <https://hackage.haskell.org/package/pandoc-types-1.17.4.2/docs/Text-Pandoc-Definition.html>


## Monkeydown

**Monkeydown** (or "Monkey Flavored Markdown") is an extension to the standard / pandoc markdown format, implemented as a pandoc filter.


## `jsinliner.py`

A filter to enable inlining of raw HTML, CSS and JavaScript code in markdown.

Examples: (embed raw HTML/CSS/JavaScript in code blocks identified by **`:html`**, **`:css`**, **`:js`** respectively)

    ```:html
    <div id="foo">
        <div id="bar">
            hello
        </div>
    </div>
    ```

    ```:css
    a {
        color: red;
    }
    ```

    ```:js
    {
        var po = document.createElement('script');
        po.type = 'text/javascript';
        po.async = true;
        po.src = 'https://apis.google.com/js/plusone.js';
        var s = document.getElementsByTagName('script')[0];
        s.parentNode.insertBefore(po, s);
    }
    ```


## `includes.hs`

A script that reads a markdown document, finds all the inline code blocks with attribute `include`, and replaces their contents with the contents of the file give.

From: <http://pandoc.org/filters.html#include-files>
