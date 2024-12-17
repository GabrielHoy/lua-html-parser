# HTML Parser for Luau and Lua5.1

This is a basic parser for HTML written in Luau, and ported to Lua5.1.<br>
This parser attempts to support all modern HTML, please create an issue if I've missed something!
<br>


**How to Use:**<br>
1) Create a new parser object using `local newParser = Parser.New(parserOptions)`<br>
2) Call `newParser:SetHTMLToParse(htmlData)` to load HTML text into the parser in preparation for processing. *This function has no returns.*<br>
3) Next, you are ready to call `newParser:ParseAsDocument()` which will return an HTML node with a tag of `root`, containing recursively built child nodes matching your HTML input data.<br>
<br>

*Going forward, references to `Parser` are the returned table from this module, `newParser` refers to a created Parser Object from `Parser.New()`, and `HTMLNode` refers to a node created by the `newParser:ParseAsDocument` or `newParser:ParseDirect` functions.*<br>

**Useful Functions:**<br>

```luau
--Returns a list of descendant nodes with a matching ID.
function Parser.GetDescendantNodesWithMatchingId(node: HTMLNode, id: string) -> {HTMLNode}

--Returns a list of descendant nodes with a matching class.
function Parser.GetDescendantNodesWithMatchingClass(node: HTMLNode, className: string) -> {HTMLNode}

--Returns a list of descendant nodes in which an attribute name exists for.
function Parser.GetDescendantNodesWithAttribute(node: HTMLNode, attributeName: string) -> {HTMLNode}

--Returns a list of descendant nodes that have a specific tag type, i.e <img> tags from tag == "img"
function Parser.GetDescendantNodesOfTagType(node: HTMLNode, tag: string) -> {HTMLNode}

--If none of the above functions suit your use case, they are all shorthand versions of using this function in some way.
--It takes a function as its second parameter, and calls that function for every descendant node of the HTMLNode supplied in the first argument.
--Returns a list of descendant nodes for which the aforementioned callback returns true for.
function Parser.GetDescendantNodesWhereCallbackIsValid(node: HTMLNode, callback: (node: HTMLNode) -> boolean?) -> {HTMLNode}

--This isn't generally *meant* to be called over newParser::ParseAsDocument, but you may find a nice use case for it.
--This function returns an array of HTMLNodes describing the exact HTML text that has been set with newParser:SetHTMLToParse()
--i.e, giving this function a string of "<head></head><body></body>" would return {HTMLNode: {tag: "head"}, HTMLNode: {tag: "body"}} whereas the ParseAsDocument function would package both nodes nicely and return {HTMLNode: {tag: "root", children: {HTMLNode: {tag: "head"}, HTMLNode: {tag: "body"}}}}
function newParser:ParseDirect(): {HTMLNode}
```
