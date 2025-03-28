<div align="center">
  <a href="https://wasmer.io" target="_blank" rel="noopener noreferrer">
    <img width="300" src="https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/logo.png" alt="Wasmer logo">
  </a>

  <p>
    <a href="https://github.com/wasmerio/wasmer/actions?query=workflow%3Abuild">
      <img src="https://github.com/wasmerio/wasmer/actions/workflows/build.yml/badge.svg?event=push" alt="Build Status">
    </a>
    <a href="https://github.com/wasmerio/wasmer/blob/master/LICENSE">
      <img src="https://img.shields.io/github/license/wasmerio/wasmer.svg" alt="License">
    </a>
    <a href="https://docs.wasmer.io">
      <img src="https://img.shields.io/static/v1?label=Docs&message=docs.wasmer.io&color=blue" alt="Wasmer Docs">
    </a>
    <a href="https://slack.wasmer.io">
      <img src="https://img.shields.io/static/v1?label=Slack&message=join%20us!&color=brighgreen" alt="Slack channel">
    </a>
  </p>

  <h3>
    <a href="https://wasmer.io/">Sito Web</a>
    <span> • </span>
    <a href="https://docs.wasmer.io">Documento</a>
    <span> • </span>
    <a href="https://slack.wasmer.io/">Chattare</a>
  </h3>

</div>

<br />

[Wasmer](https://wasmer.io/) offre contenitori ultra-leggeri basati su [WebAssembly](https://webassembly.org/) , che possono funzionare ovunque: dal desktop al cloud e persino sui dispositivi IoT, e possono essere integrati in [*qualsiasi linguaggio di programmazione*](https://github.com/wasmerio/wasmer#language-integrations).

> Più linguaggi Readme: [🇩🇪 Deutsch-Tedesco](https://github.com/wasmerio/wasmer/blob/master/docs/de/README.md) • [🇬🇧 English-Inglese](https://github.com/wasmerio/wasmer/blob/master/README.md) • [🇪🇸 Español-Spagnolo](https://github.com/wasmerio/wasmer/blob/master/docs/es/README.md) • [🇫🇷 Français-Francese](https://github.com/wasmerio/wasmer/blob/master/docs/fr/README.md) • [🇯🇵 日本語-Giapponese](https://github.com/wasmerio/wasmer/blob/master/docs/ja/README.md).

## Caratteristica

* **Veloce e sicuro**. Wasmer esegue WebAssembly a una velocità quasi nativa in un ambiente completamente controllato (bac à sable, sandbox).

* **Pluggabile**. Wasmer può supportare diversi framework di compilazione in base alle tue esigenze (LLVM，Cranelift ...).

* **Universale**. Puoi eseguire Wasmer su qualsiasi piattaforma (macOS, Linux e Windows) e processore. 

* **Conforme agli standard**. Wasmer supera la[suite di test ufficiale di WebAssembly](https://github.com/WebAssembly/testsuite) supportando[WASI](https://github.com/WebAssembly/WASI) e [Emscripten](https://emscripten.org/).

## Inizio rapido

Wasmer non richiede l'installazione di altre dipendenze. Puoi utilizzare il seguente programma di installazione per procedere con l'installazione:

```sh
curl https://get.wasmer.io -sSfL | sh
```

<details>
  <summary>Utilizzando Powershell (Windows)</summary>
  <p>

```powershell
iwr https://win.wasmer.io -useb | iex
```

</p>
</details>

> Per ulteriori opzioni di installazione, consulta [wasmer-install](https://github.com/wasmerio/wasmer-install): Homebrew, Scoop, Cargo...


#### Eseguire un file WebAssembly

Dopo aver installato Wasmer, sei pronto per eseguire il tuo primo file WebAssembly! 🎉

Puoi iniziare eseguendo QuickJS: [qjs.wasm](https://registry-cdn.wapm.io/contents/_/quickjs/0.0.3/build/qjs.wasm)

```bash
$ wasmer qjs.wasm
QuickJS - Type "\h" for help
qjs > const i = 1 + 2;
qjs > console.log("hello " + i);
hello 3
```

#### Ecco cosa puoi fare:

- [Utilizzare Wasmer nella tua applicazione Rust](https://docs.wasmer.io/integrations/rust)
- [Pubblicare un pacchetto Wasm su WAPM](https://docs.wasmer.io/ecosystem/wapm/publishing-your-package)
- [Per saperne di più su Wasmer](https://medium.com/wasmer/)

## Integrazione linguistica

📦 Wasmer runtime può essere incorporato come libreria in **diverse lingue**，permettendoti di utilizzare WebAssembly ovunque.

| &nbsp; | Lingua | Pacchetto | Documento |
|-|-|-|-|
| ![Rust logo] | [**Rust**][Rust integration] | [`wasmer` Rust crate] | [Documento][rust docs]
| ![C logo] | [**C/C++**][C integration] | [`wasmer.h` headers] | [Documento][c docs] |
| ![C# logo] | [**C#**][C# integration] | [`WasmerSharp` NuGet package] | [Documento][c# docs] |
| ![D logo] | [**D**][D integration] | [`wasmer` Dub package] | [Documento][d docs] |
| ![Python logo] | [**Python**][Python integration] | [`wasmer` PyPI package] | [Documento][python docs] |
| ![JS logo] | [**Javascript**][JS integration] | [`@wasmerio` NPM packages] | [Documento][js docs] |
| ![Go logo] | [**Go**][Go integration] | [`wasmer` Go package] | [Documento][go docs] |
| ![PHP logo] | [**PHP**][PHP integration] | [`wasm` PECL package] | [Documento][php docs] |
| ![Ruby logo] | [**Ruby**][Ruby integration] | [`wasmer` Ruby Gem] | [Documento][ruby docs] |
| ![Java logo] | [**Java**][Java integration] | [`wasmer/wasmer-jni` Bintray package] | [Documento][java docs] |
| ![Elixir logo] | [**Elixir**][Elixir integration] | [`wasmex` hex package] | [Documento][elixir docs] |
| ![R logo] | [**R**][R integration] | *Nessun pacchetto software pubblicato* | [Documento][r docs] |
| ![Postgres logo] | [**Postgres**][Postgres integration] | *Nessun pacchetto software pubblicato* | [Documento][postgres docs] |
|  | [**Swift**][Swift integration] | *Nessun pacchetto software pubblicato* | |
| ![Zig logo] | [**Zig**][Zig integration] | *no published package* | |
| ![Ocaml logo] | [**OCaml**][OCaml integration] | [`wasmer` OCaml package] | |

[👋 Manca la lingua?](https://github.com/wasmerio/wasmer/issues/new?assignees=&labels=%F0%9F%8E%89+enhancement&template=---feature-request.md&title=)

[rust logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/rust.svg
[rust integration]: https://github.com/wasmerio/wasmer/tree/master/lib/api
[`wasmer` rust crate]: https://crates.io/crates/wasmer/
[rust docs]: https://docs.rs/wasmer/

[c logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/c.svg
[c integration]: https://github.com/wasmerio/wasmer/tree/master/lib/c-api
[`wasmer.h` headers]: https://wasmerio.github.io/wasmer/c/
[c docs]: https://docs.rs/wasmer-c-api/*/wasmer/wasm_c_api/index.html

[c# logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/csharp.svg
[c# integration]: https://github.com/migueldeicaza/WasmerSharp
[`wasmersharp` nuget package]: https://www.nuget.org/packages/WasmerSharp/
[c# docs]: https://migueldeicaza.github.io/WasmerSharp/

[d logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/d.svg
[d integration]: https://github.com/chances/wasmer-d
[`wasmer` Dub package]: https://code.dlang.org/packages/wasmer
[d docs]: https://chances.github.io/wasmer-d

[python logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/python.svg
[python integration]: https://github.com/wasmerio/wasmer-python
[`wasmer` pypi package]: https://pypi.org/project/wasmer/
[python docs]: https://github.com/wasmerio/wasmer-python#api-of-the-wasmer-extensionmodule

[go logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/go.svg
[go integration]: https://github.com/wasmerio/wasmer-go
[`wasmer` go package]: https://pkg.go.dev/github.com/wasmerio/wasmer-go/wasmer
[go docs]: https://pkg.go.dev/github.com/wasmerio/wasmer-go/wasmer?tab=doc

[php logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/php.svg
[php integration]: https://wasmerio.github.io/wasmer-php/
[`wasm` pecl package]: https://pecl.php.net/package/wasm
[php docs]: https://wasmerio.github.io/wasmer-php/wasm/

[js logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/js.svg
[js integration]: https://github.com/wasmerio/wasmer-js
[`@wasmerio` npm packages]: https://www.npmjs.com/org/wasmer
[js docs]: https://docs.wasmer.io/integrations/js/reference-api

[ruby logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/ruby.svg
[ruby integration]: https://github.com/wasmerio/wasmer-ruby
[`wasmer` ruby gem]: https://rubygems.org/gems/wasmer
[ruby docs]: https://www.rubydoc.info/gems/wasmer/

[java logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/java.svg
[java integration]: https://github.com/wasmerio/wasmer-java
[`wasmer/wasmer-jni` bintray package]: https://bintray.com/wasmer/wasmer-jni/wasmer-jni
[java docs]: https://github.com/wasmerio/wasmer-java/#api-of-the-wasmer-library

[elixir logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/elixir.svg
[elixir integration]: https://github.com/tessi/wasmex
[elixir docs]: https://hexdocs.pm/wasmex/api-reference.html
[`wasmex` hex package]: https://hex.pm/packages/wasmex

[r logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/r.svg
[r integration]: https://github.com/dirkschumacher/wasmr
[r docs]: https://github.com/dirkschumacher/wasmr#example

[postgres logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/postgres.svg
[postgres integration]: https://github.com/wasmerio/wasmer-postgres
[postgres docs]: https://github.com/wasmerio/wasmer-postgres#usage--documentation

[swift integration]: https://github.com/AlwaysRightInstitute/SwiftyWasmer

[zig logo]: https://raw.githubusercontent.com/ziglang/logo/master/zig-favicon.png
[zig integration]: https://github.com/zigwasm/wasmer-zig

[OCaml logo]: https://raw.githubusercontent.com/wasmerio/wasmer/master/assets/languages/ocaml.svg
[OCaml integration]: https://github.com/wasmerio/wasmer-ocaml
[`wasmer` OCaml package]: https://opam.ocaml.org/packages/wasmer/

## Contributo

**Accogliamo con favore ogni forma di contributo, specialmente quelli provenienti dai nuovi membri della comunità.** 💜

Puoi verificare come compilare Wasmer nella[nostra documentazione](https://docs.wasmer.io/ecosystem/wasmer/building-from-source)!

### 测试

想要测试吗?  [参考 Wasmer 文档](https://docs.wasmer.io/ecosystem/wasmer/building-from-source/testing).

## 社区

Wasmer 拥有一个由出色的开发人员和贡献者组成的社区。 欢迎你，请加入我们! 👋

### 频道

- [Slack](https://slack.wasmer.io/)
- [Twitter](https://twitter.com/wasmerio)
- [Facebook](https://www.facebook.com/wasmerio)
- [Email](mailto:hello@wasmer.io)
