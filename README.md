iso8583_erl
=====

An erlang library for processing iso8583 messages inspired by [erl8583],[jpos library].

This is a minimalist library for  packing and unpacking iso8583 financial messages.

It accepts a module which contains specification information about fields,mti,bitmap  for your interchange.

It then uses those specifications to then pack and unpack iso messages for your interchange.

There is currently only one marshaller which is for ascii but binary is being worked on.


## quickstart ##


## description ##


## exports ##



## acknowledgements ##
i wish to thank the creators of the jpos library from which i took many nice ideas.
i also wish to thank the creators of erl8583 library which gave a lot of excellent ideas.

[erl8583]: https://github.com/mgwidmann/erl8583
[jpos library]: https://github.com/jpos/jPOS
