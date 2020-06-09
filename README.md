# conduit.purs
Implementation of a Conduit front-end in purescript. Live at [sylvainleclercq.com/conduit.purs/](https://sylvainleclercq.com/conduit.purs/)

[Conduit](https://github.com/gothinkster/realworld) is a collection of front and back ends for a clone of the blogging website [Medium](https://medium.com/).  
This repository contains my own implementation of Conduit, in Purescript with Halogen.

Note that at this time, the site uses a public testing API in the back end, most of the content is therefore garbage. You can freely register as many accounts as you wish and play with the front-end, the email address required for registration is not checked.

## Features
* Login/logout & registration, with token based authentication. Automatic login of previously authenticated user.
* Display recent articles from the global feed/the user's feed/matching a tag
* Article creation, edition and deletion, with embedded markdown editor
* Markdown article rendering
* Following users 
* Favoriting articles
* Commenting on articles/comment deletion. Comment edition does not appear to be supported by the public back end.

## Dependencies
On top of Halogen and other Purescript libraries, the front end uses [SimpleMDE](https://github.com/sparksuite/simplemde-markdown-editor) and [Marked](https://github.com/markedjs/marked) for Markdown editing and rendering.
