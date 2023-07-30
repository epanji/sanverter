<p align="center">
    <img src="sanverter.png" alt="LOGO" width="300px" height="150px">
</p>

# SANVERTER

**SRT⟷ASS Converter**

Convert SRT file to ASS file and vise versa.

Author: _Panji Kusuma (epanji@gmail.com)_

### Drawbacks

- Detail timing will change due to milliseconds and centiseconds different.
- Information such as title, style and some modifiers will loss due to conversion.
- Only **bold**, *italic*, <u>underline</u>, <s>strikeout</s> and <font color="#00FFFF">colors</font> will be preserve.
- Global styles did not preserve like local modifiers.

## Dependencies

- [esrap](https://github.com/scymtym/esrap)
- [claraoke](https://github.com/epanji/claraoke)

## Primary Features

### Converting

Convert file.srt to file.ass with adjustment options:

```lisp
SANVERTER> (convert-srt-to-ass #p"bad-source.srt" :output #p"tmp.ass" :title "Temporary" :primary-colour "blue violet")
#P"tmp.ass"
```
Convert file.ass to file.srt with optional output:

```lisp
SANVERTER> (convert-ass-to-srt #p"bad-source.ass" :output #p"tmp.srt")
#P"tmp.srt"
```

## Secondary Features

### Parsing

Parsing SRT format:

```lisp
SANVERTER> (parse-subrip #p"bad-source.srt")
```

Parsing VTT format:

```lisp
SANVERTER> (parse-subrip #p"bad-source.vtt")
```

Parsing ASS format:

```lisp
SANVERTER> (parse-subass #p"bad-source.ass")
```

### Printing

Printing SRT format:

```lisp
SANVERTER> (print-subrip #p"bad-source.srt")
```

Printing VTT format:

```lisp
SANVERTER> (print-subrip #p"bad-source.vtt" "en")
```

Printing ASS format:

```lisp
SANVERTER> (print-subass #p"bad-source.ass")
```

### Cross Printing

Printing SRT format:

```lisp
SANVERTER> (print-subrip #p"bad-source.ass")
```
```lisp
SANVERTER> (print-subrip #p"bad-source.vtt")
```

Printing VTT format:

```lisp
SANVERTER> (print-subrip #p"bad-source.ass" "en")
```
```lisp
SANVERTER> (print-subrip #p"bad-source.srt" "en")
```

Printing ASS format:

```lisp
SANVERTER> (print-subass #p"bad-source.srt")
```
```lisp
SANVERTER> (print-subass #p"bad-source.vtt")
```

### Auto Corrections

Printing from bad source will be corrected.

<table border="0" cellspacing="0" cellpadding="20">
<thead>
<tr valign="top" align="left">
<th><em>Before</em></th>
<th><em>After</em></th>
</tr>
</thead>
<tbody>
<tr valign="top">
<td>

```
1
00:00:00,000 --> 00:01:20,000
Hello world!

00:01:20,000-->  00:02:40,000
<font color=#1A2B3C>Hello <b>world</b>!</font>

3
00:02:40,000 --> 00:01:20,000
<font color="#ABC">Hello <b>world!
Hello world!

```

</td>
<td>

```
1
00:00:00,000 --> 00:01:20,000
Hello world!

2
00:01:20,000 --> 00:02:40,000
<font color="#1A2B3C">Hello <b>world</b>!</font>

3
00:02:40,000 --> 00:04:00,000
<font color="#AABBCC">Hello <b>world!
Hello world!</b></font>

```

</td>
</tr>
<tr valign="top">
<td>

```
WEBVTT

STYLE
::cue(.aa) {
           color: red;
}
::cue(.bb) {
           color: #1a2b3d;
}

00:00:00.000 --> 00:01:20.000
Hello world!

00:01:20.000 --> 00:02:40.000
<c.aa>Hello <b>world</b>!</c>

00:02:40.000 --> 00:01:20.000
<c.magenta>Hello <b>world!
Hello world!</b></c>

```

</td>
<td>

```
WEBVTT
Kind: captions
Language: en

STYLE
::cue(.ff0000) { color: #ff0000; }
::cue(.ff00ff) { color: #ff00ff; }

00:00:00.000 --> 00:01:20.000
Hello world!

00:01:20.000 --> 00:02:40.000
<c.ff0000>Hello <b>world</b>!</c>

00:02:40.000 --> 00:04:00.000
<c.ff00ff>Hello <b>world!
Hello world!</b></c>

```

</td>
</tr>
</tbody>
</table>

## Swing GUI

This is **SANVERTER** when running with **ABCL** (Armed Bear Common Lisp) in **JVM** (Java Virtual Machine)

![SWING](swing-gui.png)

## Command Line Interface

This is command line interface build with **ECL** (Embeddable Common Lisp).

Conversions

```sh
$ ./sanverter -f vtt ../bad-source.srt

Success converting ../bad-source.srt to ../bad-source.vtt.

$ ./sanverter -f srt ../bad-source.vtt

File ../bad-source.srt already exists.

$ ./sanverter -e nil -f srt ../bad-source.vtt
1
00:00:00,000 --> 00:01:20,000
Hello world!

2
00:01:20,000 --> 00:02:40,000
<font color="#1A2B3C">Hello <b>world</b>!</font>

3
00:02:40,000 --> 00:04:00,000
<font color="#AABBCC">Hello <b>world!
Hello world!</b></font>

```

Corrections

```sh
$ ./sanverter -f srt ../bad-source.srt

Convert nothing for ../bad-source.srt with option '-f srt' or '--format srt'.
If you are sure about this, add option '-c' or '--corrections'.

$ ./sanverter -c -f srt ../bad-source.srt

Success converting ../bad-source.srt to ../bad-source-rev.srt.

```

For more options:

```sh
$ ./sanverter --help
```

## License

[BSD 2-Clause License](LICENSE)

