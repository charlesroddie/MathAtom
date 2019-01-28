namespace MathDisplay.MathAtom

module LaTeXDefaultMaps =
    open MathDisplay.DataTypes

    //Use (Alt+Left mouse) drag to create multiple cursors so that you can input spaces simultaneously
    let delimiters =
       [".",           [],            ""; // . means no delimiter
        "(",           [],            "(";
        ")",           [],            ")";
        "[",           [],            "[";
        "]",           [],            "]";
        "{",           ["lbrace"],    "{";
        "}",           ["rbrace"],    "}";
        "<",           ["langle"],    "\u2329";
        ">",           ["rangle"],    "\u232A";
        "/",           [],            "/";
        "\\",          ["backslash"], "\\";
        "|",           ["vert"],      "|";
        "||",          ["Vert"],      "\u2016";
        "uparrow",     [],            "\u2191";
        "downarrow",   [],            "\u2193";
        "updownarrow", [],            "\u2195";
        "Uparrow",     [],            "\u21D1";
        "Downarrow",   [],            "\u21D3";
        "Updownarrow", [],            "\u21D5";
        "lgroup",      [],            "\u27EE";
        "rgroup",      [],            "\u27EF";
        "lceil",       [],            "\u2308";
        "rceil",       [],            "\u2309";
        "lfloor",      [],            "\u230A";
        "rfloor",      [],            "\u230B"]
        
        |> List.map (fun (k, ks, v) -> k, ks, Delimiter v) |> AliasMap.ofList