# js-dependency-injector.el #

A package which adds various utility functions around the use of `require.js`, such as injecting classes into the require block, requiring classes relatively, requiring node modules defined in package.json.

This is built to work with my other package, [`projectable.el`](https://github.com/domtronn/projectable.el), which can be used to for project navigation but ultimately builds the data structure required for this pacakge.

## Installation ##

Clone this repository and add it to Emac's `load-path`. Then require the file:
```
(add-to-list 'load-path "/path/to/cloned/repo")
(require js-dependency-injector)
```
Then enable it in your `js-mode` using
```
(add-hook 'js-mode-hook 'js-injector-minor-mode)
```

# Usage

If you enable `js-injector-minor-mode` you will have access to the following keybindings:

Key Binding | Command & Effect
-------- | --- 
`C-c C-j i` | `inject-dependency-at-point`
 | Try to inject the `class` under point from require modules _(requires a `projectable` project)_
`C-c C-j s` | `sort-dependencies`
 | Arrange the require block in alphabetical order
`C-c C-j u` | `update-dependencies`
 | Update all the dependencies in the requires function callback and update the require block _(requires a `projectable` project)_
`C-c C-j u` | `indent-require-block`
 | Indents the require block according to mode
`C-c C-j r` | `require-dependency-at-point`
 | Try to expand the `class` as a relative path at point _(requires a `projectable` project)_
`C-c C-j C-r` | `require-node-module-at-point`
 | Prompt for node modules defined in your `package.json` and require

# Examples #

## `require-node-module-at-point` ##

Given working on a node package with the following `package.json`

```json
...
"dependencies": {
  "express": "1.4.5",
  "request": "0.5.7",
  "package-name": "10.2.9"
}
...
```

Calling the `require-node-module-at-point` will prompt the user with `express, request, package-name`.
Selecting one of the options will expand point into
```javascript
var express = require('express');
```
Also, pacakges with dashes in their name _(e.g. `package-name` above)_ will be sanitised like this
```javascript
var packageName = require('package-name');
```

## `require-dependency-at-point` ##

Given that you're in a project file of a [`projectable`](https://github.com/domtronn/projectable.el) project

Calling the `require-dependency-at-point` over the `classFileName` below,
will expand the path to the path relative to the current file like this
```javascript
var class = require('classFileName');
var class = require('../../folder/classFileName');
```

## `inject-dependency-at-point` ##

Given that you're in a project file of a [`projectable`](https://github.com/domtronn/projectable.el) project and in a javascript class that uses `require.js` blocks that looks something like this.

```javascript
require([
  'project/myclass'
], function (MyClass) {
...
  var class = new Image();
...
})
```

Calling `inject-dependency-at-point` over the `new Image()` will require the class _(if it exists in the project)_ like this

```javascript
require([
  'project/myclass',
  'project/path/to/image'
], function (MyClass, Image) {
...
  var class = new Image();
...
})
```

If you have configured the [`projectable`](https://github.com/domtronn/projectable.el) `project.json` with some libraries, like this
```json
"libs": { "id":"library", "dir":"path/to/library"}
```
And try to include a class which exists in _both_ the library and the root project, the user will be prompted with the two choices, e.g. `project/path/to/image, library/path/to/image `
Selecting one of these will inject the selected one as above, so if the user chose the `image` class in the library it would expand to something like this
```javascript
require([
  'project/myclass',
  'library/path/to/image'
], function (MyClass, Image) {
...
  var class = new |Image();
...
})
```

