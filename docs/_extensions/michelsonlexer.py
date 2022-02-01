from pygments.lexer import RegexLexer
from pygments.token import Whitespace, Literal, Punctuation, Keyword, Comment


class MichelsonLexer(RegexLexer):
    name = 'Michelson'
    aliases = ['michelson']
    filenames = ['*.tz']

    tokens = {
        'root': [
            (r'\s+', Whitespace),
            # numbers
            (r'-?[0-9]+', Literal.Number),
            # raw bytes
            (r'0x[a-fA-F0-9]+', Literal.Number),
            (r'[{;,}()]', Punctuation),
            # primitives
            (r'[A-Z][_A-Z0-9]+', Keyword.Reserved),
            # constructors
            (r'[A-Z][a-z]+', Keyword.Constant),
            (r'\"[^\"]*\"', Literal.String),
            # single-line comments
            (r'#.*$', Comment.Single),
            # c-style comments
            (r'/\*', Comment.Multiline, 'comment'),
            # sections
            (r'(code|parameter|storage)', Keyword),
            # types
            (r'[a-z][a-z_]*', Keyword.Type),
            # annotations
            (r'[@:%](@|%|%%|[_a-zA-Z][_0-9a-zA-Z\.]*)?', Keyword.Name),
        ],
        'comment': [
            (r'[^*/]', Comment.Multiline),
            (r'/\*', Comment.Multiline, '#push'),
            (r'\*/', Comment.Multiline, '#pop'),
            (r'[*/]', Comment.Multiline),
        ],
    }


def setup(sphinx):
    sphinx.add_lexer("michelson", MichelsonLexer)
    return {'parallel_read_safe': True}
