[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![CELPA](https://celpa.conao3.com/packages/meta-view-badge.svg)](https://celpa.conao3.com/#/meta-view)

# meta-view
> View metadata from .NET assemblies

![CI](https://github.com/emacs-vs/meta-view/workflows/CI/badge.svg)

<p align="center">
  <img src="./etc/demo.png" width="618" height="307" />
</p>

View assembly metadata, and it's documentation from a C# project, generated
by Visual Studio IDE (not VSCode).

*P.S. This package mimics the [MetadataSource](https://github.com/dotnet/roslyn/tree/main/src/Features/Core/Portable/MetadataAsSource)
feature from Visual Studio IDE*

## :floppy_disk: Usage

### :mag: 1. Open a C# file

Make sure the `.cs` file is a valid source file under a C# project.

### :mag: 2. View assembly metadata!

Navigate to a symbol you want to search, then call `M-x meta-view-at-point`.

## :hammer: Configurations

#### `meta-view-active-modes`

Major modes that allow to view metadata source, default is `csharp-mode`.

#### `meta-view-after-insert-hook`

Hooks run after buffer is inserted to display view, default is `nil`.

For example, display line numbers for metadata buffer. (on left)

```el
(add-hook 'meta-view-after-insert-hook
          (lambda ()
            (display-line-numbers-mode 1)))
```

| Display Line Numbers         | Advance                       |
|------------------------------|-------------------------------|
| <img src="./etc/dis-ln.png"> | <img src="./etc/advance.png"> |

You can fold all comments by your favorite folding package. (on right)

#### `meta-view-display-function`

Function call to display reference data, default to `#'switch-to-buffer`.

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
