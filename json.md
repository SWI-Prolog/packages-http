# Supporting JSON			{#jsonsupport}

From  http://json.org, "
JSON (JavaScript Object Notation) is a lightweight data-interchange
format. It is easy for humans to read and write. It is easy for machines
to parse and generate. It is based on a subset of the JavaScript
Programming Language, Standard ECMA-262 3rd Edition - December 1999.
JSON is a text format that is completely language independent but uses
conventions that are familiar to programmers of the C-family of
languages, including C, C++, C#, Java, JavaScript, Perl, Python, and
many others. These properties make JSON an ideal data-interchange
language."

Although JSON is nowadays used a lot outside the context of web
applications, SWI-Prolog's support for JSON started life as part of the
HTTP package. SWI-Prolog supports two Prolog representations for JSON
terms. The first and oldest map JSON objects to a term
json(PropertyList) and use the `@` functor to disambiguate e.g. `null`
from the string `"null"`, leading to `@(null)`. As of SWI-Prolog version
7, JSON objects may be represented using _dict_ objects and JSON strings
using Prolog strings. Predicates following this convention are suffixed
with ``_dict``, e.g. json_read_dict/2.  For example, given the JSON document

    { "name": "Bob", "children": ["Mary", "John"], "age":42, "married": true }

we get either (using json_read/2):

    json([name='Bob', children=['Mary', 'John'], age=42, married= @(true)]).

or (using json_read_dict/2):

    _{age:42, children:["Mary", "John"], married:true, name:"Bob"}


The SWI-Prolog JSON interface consists of three libraries:

    * library(http/json) provides support for the core JSON object
      serialization and parsing.
    * library(http/json_convert) converts between the primary
      representation of JSON terms in Prolog and more application
      oriented Prolog terms.  E.g. point(X,Y) vs. object([x=X,y=Y]).
    * library(http/http_json) hooks the conversion libraries into
      the HTTP client and server libraries.

    [[library(http/json)]]
    [[library(http/json_convert)]]
    [[library(http/http_json)]]

