       var token;
        while (this.lookahead) {
            --this.lookahead;
            this.tokenIndex = (this.tokenIndex + 1) & 3;
            token = this.tokens[this.tokenIndex];
            if (token.type != NEWLINE || this.scanNewlines)
                return token.type;
        }

        for (;;) {
            var input = this.input;
            var match = (this.scanNewlines ? /^[ \t]+/ : /^\s+/)(input);
            if (match) {
                var spaces = match[0];
                this.cursor += spaces.length;
                var newlines = spaces.match(/\n/g);
                if (newlines)
                    this.lineno += newlines.length;
                input = this.input;
            }

            if (!(match = /^\/(?:\*(?:.|\n)*?\*\/|\/.*)/(input)))
                break;
            var comment = match[0];
            this.cursor += comment.length;
            newlines = comment.match(/\n/g);
            if (newlines)
                this.lineno += newlines.length
        }

        this.tokenIndex = (this.tokenIndex + 1) & 3;
        token = this.tokens[this.tokenIndex];
        if (!token)
            this.tokens[this.tokenIndex] = token = {};

        if (!input)
            return token.type = END;

        if ((match = fpRegExp(input))) {
            token.type = NUMBER;
            token.value = parseFloat(match[0]);
        } else if ((match = /^0[xX][\da-fA-F]+|^0[0-7]*|^\d+/(input))) {
            token.type = NUMBER;
            token.value = parseInt(match[0]);
        } else if ((match = /^\w+/(input))) {
            var id = match[0];
            token.type = keywords[id] || IDENTIFIER;
            token.value = id;
        } else if ((match = /^"(?:\\.|[^"])*"|^'(?:[^']|\\.)*'/(input))) { //"){
            token.type = STRING;
            token.value = eval(match[0]);
        }

        if (this.scanOperand &&        
           (match = /^\/((?:\\.|[^\/])+)\/([gi]*)/(input))) {
            token.type = REGEXP;
            token.value = new RegExp(match[1], match[2]);
        }

        token.start = this.cursor;
        this.cursor += match[0].length;
        token.end = this.cursor;
        token.lineno = this.lineno;
        return token.type;