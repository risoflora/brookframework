[![License: LGPL v2.1](https://img.shields.io/badge/License-LGPL%20v2.1-lemmon.svg)](https://github.com/risoflora/brookframework/blob/master/LICENSE)
[![GitHub releases](https://img.shields.io/github/v/release/risoflora/brookframework?color=lemmon)](https://github.com/risoflora/brookframework/releases)
[![Support this project via PayPal](https://img.shields.io/badge/donate-paypal-brightgreen)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=silvioprog%40gmail%2ecom&lc=US&item_name=brookframework&item_number=brookframework&currency_code=USD&bn=PP%2dDonationsBF%3aproject%2dsupport%2ejpg%3aNonHosted)

# Overview

Brook is a cross-platform microframework which helps to develop web Pascal applications built by [Delphi](<https://en.wikipedia.org/wiki/Delphi_(software)>) or [Lazarus IDE and Free Pascal](<https://en.wikipedia.org/wiki/Lazarus_(IDE)>). Its core has been developed using the [Sagui library](https://risoflora.github.io/libsagui), that's why it is so fast, compact and useful to run on embedded systems.

# Features

- **Three threading modes:**
  - Event-driven - single-thread + polling.
  - Threaded - one thread per request.
  - Polling - pre-allocated threads.
  - Isolated request - request processed outside main thread.
- **Fast path routing that supports:**
  - Regular expression with [JIT](https://www.pcre.org/current/doc/html/pcre2jit.html) optimization.
  - [Binary search](https://en.wikipedia.org/wiki/Binary_search_algorithm) for path entry-points.
- **HTTP compression:**
  - [Deflate](https://en.wikipedia.org/wiki/DEFLATE) - for static strings and streaming.
  - [Gzip](https://en.wikipedia.org/wiki/Gzip) - for file compression.
- **HTTP cookies:**
  - Providing classes which handles server side cookies.
- **HTTPS support:**
  - Data encryption through [GnuTLS](https://www.gnutls.org) library.
- **Dual stack:**
  - IPv4 and IPv6 on top of a single socket.
- **Basic authentication:**
  - For standard login using username and password.
- **Upload/download:**
  - Static body and payload.
  - Content streaming for real-time applications.
  - Small and large files transferring.
- **Mathematical expression evaluator:**
  - Arithmetic, bitwise and logical operators.
  - Variables allocation at build and/or run time.
  - Macro support to define functions at run time.
  - Extendable with custom functions.
  - Error handling with error kind and position.
- **Media types:**
  - Resolving media types ([MIME](https://en.wikipedia.org/wiki/MIME)) in any supported platform.
- **String buffer:**
  - For fast operations involving strings.
- **String map:**
  - Hashed lists for key-value mapping.
- **And more:**
  - Discover more features by playing with our examples.

# Examples

The example below shows a minimal `hello world` HTTP server:

```delphi
type
  THTTPServer = class(TBrookHTTPServer)
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  end;

procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  AResponse.Send('Hello world', 'text/plain', 200);
end;

begin
  with THTTPServer.Create(nil) do
  try
    Port := 8080;
    if not Active then
      Exit;
    WriteLn('Server running at http://localhost:', Port);
    Open;
    ReadLn;
  finally
    Free;
  end;
end.
```

There are other examples available in the [`Examples/`](https://github.com/risoflora/brookframework/tree/master/Examples) directory.

# Targets

Successfully tested on:

- Windows
- Linux
- Raspbian/Android

compiled using:

- Delphi XE family (Rio)
- Lazarus / Free Pascal (Lazarus 2.0+ / FPC 3.2+)

# Versioning

Starting from the version 1.0.0, Brook follows the [SemVer](https://semver.org) rules regarding API changes with backwards compatibility across major releases.

# Licensing

Brook framework is released under GNU Lesser General Public License v2.1. Check the [LICENSE file](https://github.com/risoflora/brookframework/blob/master/LICENSE) for more details.

# Documentation

The documentation has been written in [PasDoc](https://github.com/pasdoc/pasdoc) and is available in HTML format at [brookframework-docs/index.html](https://risoflora.github.io/brookframework-docs/index.html).

# Downloading

All stable releases are available for download at the [releases page](https://github.com/risoflora/brookframework/releases).

To download the very latest source from the Git server, do this:

```bash
git clone https://github.com/risoflora/brookframework.git
```

It will create a directory named `brookframework` filled with the source code.

# Contributing

Brook framework is totally open source and would not be possible without our [contributors](https://github.com/risoflora/brookframework/blob/master/THANKS). If you want to submit contributions, please fork the project on GitHub and send a pull request. You retain the copyright on your contributions. If you have questions, open a new issue at the [issues page](https://github.com/risoflora/brookframework/issues). For donations to support this project, please click the button below.

[![Support this project via PayPal](https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=silvioprog%40gmail%2ecom&lc=US&item_name=brookframework&item_number=brookframework&currency_code=USD&bn=PP%2dDonationsBF%3aproject%2dsupport%2ejpg%3aNonHosted)

If you are Brazilian and want to donate in BRL, please use [this link](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=GE9VT768TLP74&item_name=brookframework&currency_code=BRL&source=url) or make an identified [bank deposit](https://drive.google.com/file/d/1CQWoDVLnepbs29enY5CylAw_omNvit5M/view?usp=sharing).

See the name of all donors in [DONORS](https://github.com/risoflora/brookframework/tree/master/DONORS) file.

# Support

This project is completely self-explanatory, but, if you need a consulting service to integrate it on your project, [contact us](mailto:silvioprog@gmail.com).
