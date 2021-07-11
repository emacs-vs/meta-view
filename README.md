[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
![CI](https://github.com/emacs-vs/meta-view/workflows/CI/badge.svg)

# meta-view
> View metadata from .NET assemblies

View assembly metadata, and it's documentation from a C# project, generated
by Visual Studio IDE (not VSCode).

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

For example, display line numbers for metadata buffer.

```el
(add-hook 'meta-view-after-insert-hook
          (lambda ()
            (display-line-numbers-mode 1)))
```

<p align="center">
  <img src="./etc/advance.png" width="463" height="204" />
</p>

#### `meta-view-display-function`

Function call to display reference data, default to `#'switch-to-buffer`.

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
