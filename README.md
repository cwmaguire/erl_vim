# erl_vim
Web vim backed by Erlang through a websocket

How hard could it be to make a text editor?
I aim to find out ... until I lose interest.

Ideas:
- web front end
    - CSS
    - DIV tag for text and highlighting
- Erlang process in the back end
    - process hierarchy for sub states
    - stand alone states with stand alone processes
    - potentially some kind of transactional command listener
        - e.g. for commands in input mode

* Copy erlang.mk
* make -f erlang.mk bootstrap-rel
* make -f erlang.mk bootstrap
* add cowboy to Makefile DEPS
* copy source from erl_mud
