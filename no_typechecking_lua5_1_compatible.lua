--[[
MIT License

Copyright (c) 2024 Gabriel Hoy

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
]]
local HTMLParser = {}
HTMLParser.__index = HTMLParser

--!TYPES
--[[
@type HTMLParser = {
    SetHTMLToParse: (html: string) -> nil, --Sets the HTML to parse, should be called before calling ParseDirect or ParseAsDocument.

    ParseDirect: () -> {HTMLNode}, --Useful if parsing sub-sections of HTML code, returns a table of nodes that are the root nodes of the parsed HTML, i.e in "<body></body><head></head>", it would reutrn {HTMLNode, HTMLNode}, one for each.
    ParseAsDocument: () -> HTMLRootNode, --Useful for parsing an entire HTML Document, you will most likely want this. Returns the root node of the parsed HTML, i.e in "<head></head><body></body>", it would return the an HTMLNode with a tag name of "root" with two children, "head" and "body".


    options: HTMLParserOptions, --The options that the parser was created with, see HTMLParserOptions for more information.
    rawHtml: string?, --The raw HTML string that was set with SetHTMLToParse
    root: HTMLRootNode?, --The root node of the parsed HTML document, only set after calling ParseAsDocument.
    parseCache: {HTMLNode}?, --If this exists then the HTML has already been parsed, used to prevent re-parsing the same HTML twice if a parse function is called twice without setting new HTML.
}

@type HTMLParserOptions = {
    lowerCaseTags: boolean?, --default: true, if true then all tags will be :lower()'ed
    lowerCaseAttributes: boolean?, --default: true, if true then all attributes will be :lower()'ed
    anonymousTextHandling: "node" | "string" | "none", --default: "node", if node then any text that is not inside a tag definition in some way will be treated as a text node, if "string" then it will be inserted directly as a string into the parent node's children, if "ignore" then it will be entirely ignored.
    anonymousTextNodeName: string?, --default: "anonymousTextNode", the name of the tag that will be used for anonymous text nodes if anonymousTextHandling is set to "node".
    parseClassAttribute: boolean?, --default: true, if true then the parser will parse the class attribute into a table of classes, if false then it will be left as a string in the "attributes" dictionary if classes are present.
    parseIdAttribute: boolean?, --default: true, if true then the parser will convert node.attributes.id if present into node.id, if false then it will be left as a string in the "attributes" dictionary if an id is present.
}

@type HTMLNode = {
    tag: string, --Lowercase tag name provided
    tagOpenStart: number?, --Numerical absolute index of the < character in the HTML document's text, if this is an anonymous text node this will be nil
    tagOpenEnd: number?, --Numerical absolute index of the > character(closing the *start* tag) in the HTML document's text, if this is an anonymous text node this will be nil
    tagCloseStart: number?, --Numerical absolute index of the < character in the HTML document's text, if the tag is self-closing, this will be nil
    tagCloseEnd: number?, --Numerical absolute index of the > character(closing the *end* tag) in the HTML document's text, if the tag is self-closing, this will be nil
    classes: {[string]: true}?, --Dictionary of classes that apply to this HTMLNode, only present if the parser option parseClassAttribute is set to true - otherwise classes will be located in the attributes table if present.
    id: string?, --String describing the current ID for this HTMLNode, only present if the parser option parseIdAttribute is set to true - otherwise the id will be located in the attributes table if present.
    content: string?, --The content of the tag, if it's an anonymous text node this will be the text content of the node - otherwise it will be nil.
    attributes: {[string]: string}, --String-indexed dictionary of attributes, attrName -> attrValue
    children: {HTMLNode}, --Array of children nodes, in the order they were specified.
    root: HTMLRootNode?, --The root node of the HTML document, useful for tree traversal... If the parser function used doesn't generate a root node, this will be nil.
    parent: HTMLNode | HTMLRootNode, --The parent node of this node - if this is the root node then node.parent == node.
}
]]

--!Luau -> Lua5.1 Compatibility

local typeof = typeof or type
local charPattern = (utf8 and utf8.charpattern) or "[%z\x01-\x7F\xC2-\xF4][\x80-\xBF]*"
local splitSpaces = string.split or function(str, _)
    str = str .. " "
    local returnedTab = {}
    for subStr in string.gmatch(str, "(.-) ") do
        table.insert(returnedTab,subStr)
    end
    return returnedTab
end

--!CONSTANTS

local DEFAULT_PARSER_OPTIONS = {
    lowerCaseTags = true,
    lowerCaseAttributes = true,
    anonymousTextHandling = "string",
    anonymousTextNodeName = "anonymousTextNode",
    parseClassAttribute = true,
    parseIdAttribute = true
}


local VOID_ELEMENTS = {
	area = true,
	base = true,
	br = true,
	col = true,
	command = true,
	embed = true,
	hr = true,
	img = true,
	input = true,
	keygen = true,
	link = true,
	meta = true,
	param = true,
	source = true,
	track = true,
	wbr = true
}
--In this parser, "Raw Text" elements are elements that should not have their contents parsed, and should be treated as a single text node - think "script".
local RAW_TEXT_ELEMENTS = {
    script = true
}

--!FUNCTIONS

local function DeepCopy(t)
    local copy = {}
    for k, v in pairs(t) do
        if typeof(v) == "table" then
            copy[k] = DeepCopy(v)
        else
            copy[k] = v
        end
    end
    return copy
end

--Does not return the node itself, but rather all of its descendant nodes that have a truthy result from the provided callback function.
--Useful for doing things like finding all nodes with a certain class, or all nodes with a certain id, etc...
function HTMLParser.GetDescendantNodesWhereCallbackIsValid(node, callback)
    local validNodes = {}
    for _,child in ipairs(node.children) do
        if typeof(child)=="table" then
            if callback(child) then
                table.insert(validNodes, child)
            end
            for _,validChildNode in ipairs(HTMLParser.GetDescendantNodesWhereCallbackIsValid(child, callback)) do
                table.insert(validNodes, validChildNode)
            end
        end
    end
    return validNodes
end

--Shorthand Function to get all descendant nodes with a matching ID
function HTMLParser.GetDescendantNodesWithMatchingId(node, id)
    return HTMLParser.GetDescendantNodesWhereCallbackIsValid(node, function(descNode)
        if descNode.id then
            return node.id == id
        elseif descNode.attributes.id then
            return node.attributes.id == id
        end
    end)
end

--Shorthand Function to get all descendant nodes with a matching class
function HTMLParser.GetDescendantNodesWithMatchingClass(node, className)
    return HTMLParser.GetDescendantNodesWhereCallbackIsValid(node, function(descNode)
        if descNode.classes then
            return descNode.classes[className] ~= nil
        elseif descNode.attributes.class then
            local classesSplit = splitSpaces(descNode.attributes.class, " ")
            return table.find(classesSplit, className) ~= nil
        end
    end)
end

--Shorthand Function to get all descendant nodes that have a specific attribute, i.e getting all nodes with a "style" attribute...
function HTMLParser.GetDescendantNodesWithAttribute(node, attributeName)
    return HTMLParser.GetDescendantNodesWhereCallbackIsValid(node, function(descNode)
        return descNode.attributes[attributeName] ~= nil
    end)
end

--Shorthand Function to get all descendant nodes of a certain type, i.e getting all <style> nodes from a document...
function HTMLParser.GetDescendantNodesOfTagType(node, tag)
    return HTMLParser.GetDescendantNodesWhereCallbackIsValid(node, function(descNode)
        return descNode.tag == tag
    end)
end




--Returns a new HTMLParser that you can call Parse functions on.
function HTMLParser.New(parserOptions)
    parserOptions = parserOptions or DeepCopy(DEFAULT_PARSER_OPTIONS)
    local newParser = setmetatable({}, HTMLParser)
    newParser.options = parserOptions

    newParser.parserCursorPosition = 0

    return newParser
end

function HTMLParser:SetHTMLToParse(html)
    self.rawHtml = html
    --Strip out the doctype if it exists, and store it for the root element later.
    local doctypeStart, doctypeEnd, doctypeStr = string.find(html:lower(), "<!doctype%s+(.-)%s*>")
    
    if doctypeStart and doctypeEnd then
        self.doctype = doctypeStr
        self.parserCursorPosition = doctypeEnd
    else
        self.parserCursorPosition = 0
    end

    
    local commentStartIdx, commentEndIdx
    local commentLocatorCursorPos = self.parserCursorPosition
    self.commentLocations = {}
    repeat
        commentStartIdx, commentEndIdx = string.find(html, "<!%-%-.-%-%->", commentLocatorCursorPos)
        if commentEndIdx then
            table.insert(self.commentLocations, {commentStartIdx, commentEndIdx})
            commentLocatorCursorPos = commentEndIdx+1
        end
    until not commentStartIdx or not commentEndIdx

    self.parseCache = false
end

function HTMLParser:CreateAnonymousTextNodeObject(text)
    if self.options.anonymousTextHandling == "node" then
        return {
            tag = self.options.anonymousTextNodeName,
            attributes = {},
            children = {},
            content = text,
            root = self.root
        }
    elseif self.options.anonymousTextHandling == "string" then
        return text
    end
end

function HTMLParser:ParseAnonymousTextIntoNode(offset: number)
    local currentOffset = offset or 1

    local startAnonymousCapture, endAnonymousCapture = string.find(self.rawHtml, ".-<", currentOffset)

    --Depending on the anonymousTextHandling option, we'll either return a node or a string. This function handles that for us.
    local returnedNode = self:CreateAnonymousTextNodeObject(self.rawHtml:sub(startAnonymousCapture, endAnonymousCapture-1))

    return returnedNode, endAnonymousCapture
end

--returns an HTMLNode along with a number representing the last index of the node in the HTML text. Should not be called directly by external code...
function HTMLParser:ParseNodeRecursive(offset)
    assert(self, "HTMLParser::ParseNodeRecursive must be called as a member function(and should not be called directly...are you looking for ParseDirect or ParseAsDocument?)")

    local node = {
        children = {},
        attributes = {}
    }

    local currentNodeNameBuilt = ""
    local buildStep = "name" --"name", then "attributes" if applicable, then "content", then "closing" - inside of the content step there may be child nodes!

    local currentAttributeKeyBuilt = ""
    local isBuildingAttributeName = true
    local wasWhitespacePreviousCharacterInAttributeNameDeclaration = false
    local currentAttributeValueBuilt = ""
    local isInsideString = false
    local isSelfClosingTag = false

    local currentOffset = offset or 1
    local strLen = string.len(self.rawHtml)


    local nextCharStart, nextCharEnd
    while currentOffset <= strLen do
        --Find the next comment in the HTML if any, and if the next character would be within a comment then skip over the entire comment!
        local nextComment = self.commentLocations[1]
        if nextComment and currentOffset >= nextComment[1] and currentOffset <= nextComment[2] then
            currentOffset = nextComment[2]+1
            table.remove(self.commentLocations,1)
            continue
        end
        
        nextCharStart, nextCharEnd = string.find(self.rawHtml, charPattern, currentOffset)
        local nextChar = self.rawHtml:sub(nextCharStart, nextCharEnd)

        if buildStep == "name" then
            if nextChar == "<" then --This serves as the first < character in the string, and should generally be what html node string starts with.
                node.tagOpenStart = nextCharEnd
            elseif nextChar == ">" then --If we directly close the tag, then we're done with the node start and can skip over attribute declarations.
                node.tagOpenEnd = nextCharEnd
                local loweredNodeName = currentNodeNameBuilt:lower()
                node.tag = self.options.lowerCaseTags and loweredNodeName or currentNodeNameBuilt
                if isSelfClosingTag or VOID_ELEMENTS[loweredNodeName] then
                    if VOID_ELEMENTS[loweredNodeName] then --This is a void element, so progress to the next character and then break the character consumer to return the node!
                        currentOffset += 1
                        break
                    else
                        error("Invalid HTML: Self-closing tag encountered for an element("..node.tag..") that is not a void element! TAG")
                    end
                else
                    buildStep = "content"
                end
            elseif nextChar == "/" then
                isSelfClosingTag = true
            elseif nextChar == " " and node.tagOpenStart then --we're transitioning from defining the tag name to defining its attributes!
                node.tag = self.options.lowerCaseTags and currentNodeNameBuilt:lower() or currentNodeNameBuilt
                buildStep = "attributes"
            else
                if node.tagOpenStart then --We don't care about characters before the first open tag character, so ignore them!
                    currentNodeNameBuilt = currentNodeNameBuilt..nextChar
                end
            end
        elseif buildStep == "attributes" then
            if nextChar == ">" and not isInsideString then --we're done defining attributes, and are closing the element!
                node.tagOpenEnd = nextCharEnd

                local loweredNodeName = currentNodeNameBuilt:lower()
                if isSelfClosingTag or VOID_ELEMENTS[loweredNodeName] then
                    if VOID_ELEMENTS[loweredNodeName] then --This is a void element, so progress to the next character and then break the character consumer to return the node!
                        currentOffset += 1
                        break
                    else
                        error("Invalid HTML: Self-closing tag encountered for an element("..node.tag..") that is not a void element! ATR")
                    end
                else
                    buildStep = "content"
                end
                --Is there an attribute that was being built when we hit the end of the tag definition? If so, add it to the attributes table!
                if currentAttributeKeyBuilt ~= "" or currentAttributeValueBuilt ~= "" then
                    local attrKey = self.options.lowerCaseAttributes and currentAttributeKeyBuilt:lower() or currentAttributeKeyBuilt
                    node.attributes[attrKey] = currentAttributeValueBuilt
                    isBuildingAttributeName = true
                    currentAttributeKeyBuilt = ""
                    currentAttributeValueBuilt = ""
                end
            elseif nextChar == "/" and not isInsideString then
                isSelfClosingTag = true
            elseif nextChar == "=" and isBuildingAttributeName then
                if isBuildingAttributeName then --This is the end of the attribute name, and the start of the attribute value.
                    if wasWhitespacePreviousCharacterInAttributeNameDeclaration then
                        wasWhitespacePreviousCharacterInAttributeNameDeclaration = false --We don't care about whitespace between the attribute name and the = sign...
                    end
                    isBuildingAttributeName = false
                else
                    error("Invalid HTML: Equals sign found in attribute value definition!")
                end
            elseif nextChar == " " and not isInsideString then
                if isBuildingAttributeName then --since we're still defining the attribute name, this is treated as defining the attribute without a value!
                    if currentAttributeKeyBuilt ~= "" then --We only care about this whitespace if we have an attribute name built to assign "nothing" to! Otherwise, this is just whitespace between two attribute names - ignore it.
                        wasWhitespacePreviousCharacterInAttributeNameDeclaration = true
                    end
                else --Whitespace when defining the attribute string, outside of a string! This is the end of the attribute value, and the start of another attribute definition(or the end of the tag)!
                    if currentAttributeValueBuilt ~= "" then --We only care about this whitespace if the attribute value has some value in it! Otherwise, this is just whitespace between an attribute name's = sign and the attribute value - ignore it.
                        local attrKey = self.options.lowerCaseAttributes and currentAttributeKeyBuilt:lower() or currentAttributeKeyBuilt
                        node.attributes[attrKey] = currentAttributeValueBuilt
                        isBuildingAttributeName = true
                        currentAttributeKeyBuilt = ""
                        currentAttributeValueBuilt = ""
                    end
                end
            elseif nextChar == '"' or nextChar == "'" and not isBuildingAttributeName then --We're either starting/ending a string, or we're inside of one! We can only start strings at the start of attribute values, in names just treat them as literal characters.
                if isInsideString and isInsideString == nextChar then --We're ending the string(and by extension the attribute)!
                    isInsideString = false
                    local attrKey = self.options.lowerCaseAttributes and currentAttributeKeyBuilt:lower() or currentAttributeKeyBuilt
                    node.attributes[attrKey] = currentAttributeValueBuilt
                    currentAttributeValueBuilt = ""
                    currentAttributeKeyBuilt = ""
                    isBuildingAttributeName = true
                elseif not isInsideString and currentAttributeValueBuilt == "" then --we can only start a new string if we're at the start of the attribute value!
                    isInsideString = nextChar
                else
                    currentAttributeValueBuilt..=nextChar
                end
            else
                if isBuildingAttributeName then --We're building the attribute name!
                    --If the last character was whitespace and this character is an attribute name character, then the last attribute had a blank value and this is the start of a new attribute name!
                    if wasWhitespacePreviousCharacterInAttributeNameDeclaration then
                        wasWhitespacePreviousCharacterInAttributeNameDeclaration = false
                        local attrKey = self.options.lowerCaseAttributes and currentAttributeKeyBuilt:lower() or currentAttributeKeyBuilt
                        node.attributes[attrKey] = currentAttributeValueBuilt
                        currentAttributeKeyBuilt = ""
                    end
                    currentAttributeKeyBuilt..=nextChar
                else--We're building the attribute value!
                    currentAttributeValueBuilt..=nextChar
                end
            end
        elseif buildStep == "content" then --At this point we can start building text nodes, or child nodes if we detect them!
            local loweredName = node.tag:lower()

            if RAW_TEXT_ELEMENTS[loweredName] then
                local rawTextCloseBeginIdx, rawTextCloseEndIdx = string.find(self.rawHtml:lower(), "</"..loweredName..">", currentOffset)
                if not rawTextCloseBeginIdx then
                    error("Invalid HTML: Raw-text element \""..loweredName.."\" was not closed!")
                end
                table.insert(node.children, self:CreateAnonymousTextNodeObject(self.rawHtml:sub(currentOffset, rawTextCloseBeginIdx-1)))

                node.tagCloseStart = rawTextCloseBeginIdx
                currentOffset = rawTextCloseBeginIdx+1 --the beginning of the closer tag, plus the < character - we want to be one step in front of the < so we add 1...
                buildStep = "closing"
                continue
            else
                if nextChar == "<" then --We're either starting a new child node, or we're ending this node!
                    local peekAheadCharacterStart, peekAheadCharacterEnd = string.find(self.rawHtml, charPattern, nextCharEnd+1)
                    if not peekAheadCharacterStart or not peekAheadCharacterEnd then
                        error("Invalid HTML: Unexpected end of string while peeking ahead in tag content < character!")
                    end
                    local peekAheadCharacter = self.rawHtml:sub(peekAheadCharacterStart, peekAheadCharacterEnd)
                    if peekAheadCharacter == "/" then --We're closing this tag!
                        buildStep = "closing"
                        node.tagCloseStart = nextCharEnd
                    else --since the next character isn't a /, assume we're starting a new child node! Recurse this function into that node definition.
                        local childNode, offsetAfterThisChildNode = self:ParseNodeRecursive(nextCharEnd)
                        table.insert(node.children, childNode)
                        childNode.parent = node
                        currentOffset = offsetAfterThisChildNode
                        continue
                    end
                else
                    if self.options.anonymousTextHandling ~= "ignore" then --If we should treat anonymous content text as child nodes in some way, then parse them out - otherwise ignore them entirely
                        local anonymousTextNode, offsetAfterAnonymousText = self:ParseAnonymousTextIntoNode(nextCharEnd)
                        table.insert(node.children, anonymousTextNode)
                        --Is the anonymous text node we just inserted a node or a string(in accordance with the HTMLParserOptions)? If it's a node, set its parent to this node!
                        if type(anonymousTextNode)=="table" then
                            anonymousTextNode.parent = node
                        end
                        currentOffset = offsetAfterAnonymousText
                        continue
                    end
                end
            end
        elseif buildStep == "closing" then --We're 1 character after the < character that started a closing tag, we know what the tag name is so we can just skip to the end of the closing!
            local closerStringStart, closerStringEnd = string.find(self.rawHtml, ">", nextCharEnd)
            if not closerStringStart or not closerStringEnd then
                error("Invalid HTML: Unexpected end of string while peeking ahead in "..currentNodeNameBuilt.." tag closing for > character! Idx: "..nextCharEnd)
            end
            local closerString = self.rawHtml:sub(nextCharEnd, closerStringEnd) --This will be the entire closing tag, sans the < character that was previously processed to flip to the "closing" buildStep.
            if closerString:lower() ~= ("/"..currentNodeNameBuilt:lower()..">") then
                error("Invalid HTML: Unexpected closing tag, expected </"..currentNodeNameBuilt:lower().."> but got <"..closerString:lower().." instead!")
            end

            node.tagCloseEnd = closerStringEnd
            currentOffset = closerStringEnd+1
            break
        end

        currentOffset = nextCharEnd + 1
    end


    if not node.tag then --If we haven't even specified a tag for the node, then we're probably dealing with the EOF...
        return nil, currentOffset
    end

    return node, currentOffset
end

--this function takes the "class" attribute if there is one present on a node, deletes the attribute, then creates a "classes" member of the node with the parsed classes.
--Do note that this function is NON-RECURSIVE and will not apply to child nodes.
function HTMLParser:ParseNodeClasses(node)
    node.classes = {}
    if node.attributes.class then
        --go through the class attributes and split them by spaces, then add every class to the classes dictionary - we index it by the class -> true to make lookup faster instead of needing table.find.
        for _,className in ipairs(splitSpaces(node.attributes.class, " ")) do
            node.classes[className] = true
        end
        node.attributes.class = nil --nil out the class attribute since we've already parsed it into the classes table.
    end

    return node
end

--Parses HTML and returns the parsed tree of HTMLNode's directly without any heirarchy additions,
--i.e if there are multiple elements specified as "root" nodes (like "<head></head><body></body>"), the returned table will contain both of them.
--You probably want ParseAsDocument instead of this function, as it will return a single root node that contains all the other nodes and can be recursed through nicely.
function HTMLParser:ParseDirect()
    assert(self.rawHtml, "HTMLParser::ParseDirect must be called after setting the HTML to parse with SetHTMLToParse.(You're probably looking for ParseAsDocument too!)")

    if self.parseCache then
        return self.parseCache
    end

    local nodes = {}

    local htmlLen = string.len(self.rawHtml)
    local currentOffset = self.parserCursorPosition
    while currentOffset <= htmlLen do
        local node, nextOffset = self:ParseNodeRecursive(currentOffset+1)
        if node then
            table.insert(nodes, node)
        end
        currentOffset = nextOffset
    end

    --Now that we've populated all the nodes, lets transform the tree in any ways necessary.
    if self.options.parseClassAttribute then --Should we replace the node.attributes.class string with the node.classes table?
        local function RecursiveParseClasses(node)
            local parsedNode = self:ParseNodeClasses(node)
            for idx,child in ipairs(parsedNode.children) do
                if type(child)=="table" then
                    parsedNode.children[idx] = RecursiveParseClasses(child)
                end
            end
            return parsedNode
        end
        for idx,node in ipairs(nodes) do
            nodes[idx] = RecursiveParseClasses(node)
        end
    end
    if self.options.parseIdAttribute then --Should we replace the node.attributes.id string with the node.id string?
        local function SetIdRecursive(node)
            if node.attributes.id then
                node.id = node.attributes.id
                node.attributes.id = nil
            end
            for idx,child in ipairs(node.children) do
                if typeof(child)=="table" then
                    node.children[idx] = SetIdRecursive(child)
                end
            end
            return node
        end
        for idx,node in ipairs(nodes) do
            nodes[idx] = SetIdRecursive(node)
        end
    end

    self.parseCache = nodes

    return nodes
end

--This is probably the function you're looking for -- it parses an HTML document and returns a single HTMLNode that serves as the root of the HTML tree.
function HTMLParser:ParseAsDocument()
    assert(self.rawHtml, "HTMLParser::ParseAsDocument must be called after setting the HTML to parse with SetHTMLToParse.")

    local rootNode = {}
    rootNode.tag = "root"
    rootNode.tagOpenStart = 1
    rootNode.doctype = self.doctype
    rootNode.tagOpenEnd = 1
    rootNode.tagCloseStart = string.len(self.rawHtml)
    rootNode.tagCloseEnd = string.len(self.rawHtml)
    rootNode.attributes = {}
    rootNode.classes = self.options.parseClassAttribute and {} or nil
    rootNode.root = rootNode
    rootNode.parent = nil

    self.root = rootNode

    rootNode.children = self:ParseDirect()
    --Go through the top-level children of the root returned by ParseDirect and set their parent to the root node.
    for _,child in ipairs(rootNode.children) do
        child.parent = rootNode
    end

    return rootNode
end

return HTMLParser
