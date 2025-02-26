[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/meta-view.svg)](https://jcs-emacs.github.io/jcs-elpa/#/meta-view)

# meta-view
> View metadata from .NET assemblies

[![CI](https://github.com/emacs-vs/meta-view/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-vs/meta-view/actions/workflows/test.yml)

<p align="center">
  <img src="./etc/demo.png" width="618" height="307" />
</p>

View assembly metadata, and it's documentation from a C# project, generated
by Visual Studio IDE (not VSCode).

*P.S. This package mimics the [MetadataSource](https://github.com/dotnet/roslyn/tree/main/src/Features/Core/Portable/MetadataAsSource)
feature from Visual Studio IDE*

## 💾 Usage

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

## 🛠️ Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!

### 🔬 Development

To run the test locally, you will need the following tools:

- [Eask](https://emacs-eask.github.io/)
- [Make](https://www.gnu.org/software/make/) (optional)

Install all dependencies and development dependencies:

```sh
eask install-deps --dev
```

To test the package's installation:

```sh
eask package
eask install
```

To test compilation:

```sh
eask compile
```

**🪧 The following steps are optional, but we recommend you follow these lint results!**

The built-in `checkdoc` linter:

```sh
eask lint checkdoc
```

The standard `package` linter:

```sh
eask lint package
```

*📝 P.S. For more information, find the Eask manual at https://emacs-eask.github.io/.*

## ⚜️ License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

See [`LICENSE`](./LICENSE.txt) for details.
