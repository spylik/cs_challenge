This is naive implementation of a challange defined in "CraftingSoftware coding challenge for Erlang Engineer.pdf"

It contain a simple Erlang HTTP service which works on port 4000 and accept http POST requests with json payload in body.

Based on paremeter bounded in URL it able to produce:
a) POST to "http://localhost:4000/sorted" - will produce sorted list of tasks;
b) POST to "http://localhost:4000/bash_script" - will produce sorted list of tasks represented as bash script.

In original PDF file there is no metntion does we actually need to execute input commands on server (it only mentioned response example),  so actual execution with `os:cmd` left out of the scope but easy to be implemented if need. 

It would be nice to add also the following features:

1. Loop detection in tasks;
2. More informative errors when something going wrong (e.g. can't parse json, loop detected, etc").

To build release:

```
git https://github.com/spylik/cs_challenge
cd cs_challenge
make
_rel/cs_challenge/bin/cs_challenge console
```

To test from console sorting of tasks:

```
curl -v -X POST http://localhost:4000/sorted -H "Content-Type: application/json" -d '{"tasks":[{"name":"task-1","command":"touch /tmp/file1"},{"name":"task-2","command":"cat /tmp/file1","requires":["task-3"]},{"name":"task-3","command":"echo \"Hello World!\" > /tmp/file1","requires":["task-1"]},{"name":"task-4","command":"rm /tmp/file1","requires":["task-2","task-3"]}]}'
```

To test from console bash script generation:

```
curl -v -X POST http://localhost:4000/bash_script -H "Content-Type: application/json" -d '{"tasks":[{"name":"task-1","command":"touch /tmp/file1"},{"name":"task-2","command":"cat /tmp/file1","requires":["task-3"]},{"name":"task-3","command":"echo \"Hello World!\" > /tmp/file1","requires":["task-1"]},{"name":"task-4","command":"rm /tmp/file1","requires":["task-2","task-3"]}]}'
```
