# `Brook framework`

[![Support this project via PayPal][paypal-badge]][1]
[![GitHub releases][releases-badge]][22]
[![License: LGPL v2.1][license-badge]](LICENSE)

## Overview

Brook is a cross-platform microframework which helps to develop web Pascal
applications built by [Delphi][5] or [Lazarus IDE and Free Pascal][6].
Its core has been developed using the [Sagui library][7], that's why it is so
fast, compact and useful to run on embedded systems.

## Features

- **Three threading modes:**
  - Event-driven - single-thread + polling.
  - Threaded - one thread per request.
  - Polling - pre-allocated threads.
  - Isolated request - request processed outside main thread.
- **Fast path routing that supports:**
  - Regular expression with [JIT][8] optimization.
  - [Binary search][9] for path entry-points.
- **HTTP compression:**
  - [Deflate][10] - for static strings and streaming.
  - [Gzip][11] - for file compression.
- **HTTP cookies:**
  - Providing classes which handles server side cookies.
- **HTTPS support:**
  - Data encryption through [GnuTLS][12] library.
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
  - Resolving media types ([MIME][13]) in any supported platform.
- **Logging:**
  - Allowing to generate logs in console or files.
- **String buffer:**
  - For fast operations involving strings.
- **String map:**
  - Hashed lists for key-value mapping.
- **And more:**
  - Discover more features by playing with our examples.

## Examples

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
    Open;
    if not Active then
      Exit;
    WriteLn('Server running at http://localhost:', Port);
    ReadLn;
  finally
    Free;
  end;
end.
```

There are other examples available in the [`Examples`](Examples) directory.

## Downloading

All stable releases are available to download via [GetIt][20], [OPM][21] and
GitHub [releases page][22].

We strongly recommend you to install Brook using GetIt or OPM, however, if you
want to download the very latest source from the Git repository, do this:

```bash
git clone https://github.com/risoflora/brookframework.git
```

It will create a directory named `brookframework` filled with the source code.

## Documentation

The documentation has been written in [PasDoc][15] and is available in HTML
format at [brookframework-docs][16].

## Targets

Successfully tested on:

- Windows
- Linux
- Raspbian/Android

compiled using:

- Delphi XE family (Rio)
- Lazarus / Free Pascal (Lazarus 2.0+ / FPC 3.2+)

## Versioning

Starting from the version 1.0.0, Brook follows the [SemVer][14] rules regarding
API changes with backwards compatibility across major releases.

## Contributing

Brook framework is totally open source and would not be possible without our
[contributors][17]. If you want to submit contributions, please fork the project
on GitHub and send a pull request. You retain the copyright on your
contributions.

## Donations

Many open source projects, large and small, receive donations to encourage their
authors, therefore, it would be not different in Brook.

All money collected from donations are invested to the purchase of study
materials. This way, directly or indirectly, all knowledge acquired in the
studies influence the spread of this project.

If you want to support this project, please choose one
of the options below to make a donation.

[![Support this project via PayPal][paypal-gif]][1] [![Support this project via PagSeguro][pagseguro-gif]][2]

(For those who would like to donate in Brazilian BRL, it can be done by a
[identified bank deposit][3] or via [PayPal Brazil][4].)

Check the list of [all donors](DONORS) that lovely supported this idea! :heart:

## Community

Would you like to ask questions and talk to more Brook users?

Join us to the [official group at Telegram][19] and be welcome!
:slightly_smiling_face:

## Support

This project values being simple, direct and self-explanatory. However, if you
need some help to integrate Brook to your application, we have the option of a
paid consulting service. [Contact us][18]!

## Projects using Brook

- [Brook Telegram][25] - Telegram plugin for Brook framework. [[MIT][26]]
- [Aproveita.App][27] - [PWA][24] application for supermarket products
  sales. [Comercial]
- [Client Web Portal][28] - Application to manage appointments,
  update demographics, download previous invoices from the RUBI medical records
  system. [Commercial]

Would you like to add your project to that list above? Feel free to open a
[new issue][23] requesting it! :-)

## Licensing

Brook framework is released under GNU Lesser General Public License v2.1. Check
the [LICENSE file](LICENSE) for more details.

[license-badge]: https://img.shields.io/badge/License-LGPL%20v2.1-lemmon.svg
[paypal-badge]: https://img.shields.io/badge/donate-paypal-brightgreen
[paypal-gif]: https://www.paypalobjects.com/en_US/GB/i/btn/btn_donateCC_LG.gif
[pagseguro-gif]: https://stc.pagseguro.uol.com.br/public/img/botoes/doacoes/120x53-doar.gif
[releases-badge]: https://img.shields.io/github/v/release/risoflora/brookframework?color=lemmon
[1]: https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=silvioprog%40gmail%2ecom&lc=US&item_name=brookframework&item_number=brookframework&currency_code=USD&bn=PP%2dDonationsBF%3aproject%2dsupport%2ejpg%3aNonHosted "PayPal link"
[2]: https://pag.ae/7WgS8EENR "PagSeguro link"
[3]: https://drive.google.com/file/d/1CQWoDVLnepbs29enY5CylAw_omNvit5M/view?usp=sharing "Nubank link"
[4]: https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=GE9VT768TLP74&item_name=brookframework&currency_code=BRL&source=url
[5]: https://en.wikipedia.org/wiki/Delphi_(software) "Delphi software"
[6]: https://en.wikipedia.org/wiki/Lazarus_(IDE) "Lazarus & Free Pascal"
[7]: https://risoflora.github.io/libsagui "Sagui library"
[8]: https://www.pcre.org/current/doc/html/pcre2jit.html "PCRE2 JIT"
[9]: https://en.wikipedia.org/wiki/Binary_search_algorithm "Binary search algorithm"
[10]: https://en.wikipedia.org/wiki/DEFLATE "DEFLATE compression"
[11]: https://en.wikipedia.org/wiki/Gzip "Gzip compression"
[12]: https://www.gnutls.org "GnuTLS library"
[13]: https://en.wikipedia.org/wiki/MIME "Multipurpose Internet Mail Extensions"
[14]: https://semver.org "Semantic Versioning"
[15]: https://github.com/pasdoc/pasdoc "PasDoc documentation tool"
[16]: https://risoflora.github.io/brookframework-docs/index.html "Brook online documentation"
[17]: https://github.com/risoflora/brookframework/blob/master/THANKS "Thanks to the people who help to maintain this project"
[18]: mailto:silvioprog@gmail.com "silvioprog at gmail dot com"
[19]: https://t.me/brookframework "Official Telegram group"
[20]: https://getitnow.embarcadero.com/brook-framework "Delphi GetIt"
[21]: https://wiki.lazarus.freepascal.org/Online_Package_Manager "Lazarus OPM"
[22]: https://github.com/risoflora/brookframework/releases
[23]: https://github.com/risoflora/brookframework/issues/new?labels=documentation&template=project_using_brook.md
[24]: https://en.wikipedia.org/wiki/Progressive_web_application "Progressive web application wiki"
[25]: https://github.com/Al-Muhandis/brook-telegram "Telegram plugin for Brook framework"
[26]: https://github.com/Al-Muhandis/brook-telegram/blob/master/LICENSE "Telegram plugin license"
[27]: https://supermercadosprimavera.com.br "PWA application for supermarket products sales"
[28]: https://github.com/risoflora/brookframework/issues/29
