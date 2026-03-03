# Perplex
---
![example_perplex](https://github.com/user-attachments/assets/ed9c3328-4885-4e55-b61f-5211a95c6869)

This is basically just a CLI tool for interacting with perplexity. 

## How does it work?
---
A python server & a chrome instance run in a virtual desktop vi xvfb. 
The server passes along your questions to the web page and returns them to the client.
It's pretty simple and still has the downside of having an extra chrome instance up. 

## Usage
--- 
As you would expect, once the client connects to the server then you can begin to query perplexity. 
There are a few commands that can be passed along to the python sever:

```
:length <short|mid|long|uncapped> - Sets the response length

:ctx <off|on> - Whether to keep prior queries in context

:exit - Exit the program
```

- `:ctx off` makes the server reload the page on every request.
- `:length` short keeps it to 2 sentences, medium to 4, long to 8, and uncapped removes the instruction given to the model. 
