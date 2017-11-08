# hapidry


[![Build Status](https://travis-ci.org/capgelka/hapidry.svg?branch=master)](https://travis-ci.org/capgelka/hapidry)
[![Release](https://img.shields.io/github/release/capgelka/hapidry.svg)](https://github.com/capgelka/hapidry/releases)

hapidry (haskell api diary) is a command line client for diary.ru api with interface in unix spirit. Feel free to open issues if something is done some other ugly way.

Currently it supports 7 actions (with 6 subcommands).

* Create new post in your blog
	* `hapidry post -m message -t subject`
	* `hapidry post -f '~/prepared_post.txt' -t subject`
	* `cat prepared_post.md | markdown | hapidry post -p -t subject`
	* `hapidry post -t subject (write post in your favorite $EDITOR)`
    * `hapidry post -m "only for whitelist" -w`
    * `hapidry post -m "save this as draft" -d`
    * `hapidry post --music "music for music field"`
    * `hapidry post --mood "mood for mood field"`
* Send new U-mail (private message) to user
    * `hapidry send someone -f '~/prepared_umail.txt' -t subject`
	* `hapidry send someone -m message -t subject`
	* `cat prepared_umail.md | markdown | hapidry send someone -p -t subject`
	* `hapidry send someone -t subject` (write message in your favorite `$EDITOR`)
* get notifications for umails, discussions and comments. There is also an example bash script in examples folder to wrap it, but it's better to use cron for this.
    * `hapidry notify`
* Create new comment for post with given id
	* `hapidry comment 12324 -m 'comment text'`
	* `hapidry comment 12324 -f '~/prepared_comment.txt'`
	* `cat prepared_comment.md | markdown | hapidry comment 12324 -p`
	* `hapidry comment 1234` (write comment in your favorite `$EDITOR`)

* Read blogs
    * `hapidry read` (read your own blog)
    * `hapidry read someone1 someone2` (read someone1 and someone2 blogs)
    * `hapidry read -r` (read posts in reversed order)
    * `hapidry read | w3m -T text/html | less` (use less to read posts rendered via w3m)

* Read umail
    * `hapidry umail` (read income umails)
    * `hapidry umail deleted` (read umails from deleted folder)
    * `hapidry umail -r` (umail in reversed order)
    * `hapidry umail | w3m -T text/html | less` (use less to read umail rendered via w3m)

* Read comments
    * `hapidry read postid` (can be number or number with "p" prefix)
    * `hapidry read postid -r` (read posts in reversed order)
    * `hapidry read postdid | w3m -T text/html | less` (use less to read posts rendered via w3m)

* There are also some additional features
    * `hapidry post -m message --tags "tagone, tag2,othertag"` (creates new post with 3 tags)
    * `hapidry post blog1 blog2 -m message` (add new post in 2 blogs with given names)
    * `hapidry send user1 user2 -m message` (send message to 2 users)

To start usage you should create config file. The default path is `~/.hapidry`, but you can choose another via `-c` option.

Config has simple key/value format

```
password = "password"
username = "diaryusername"
```

If you need to make actions from different user for one time, don't edit config, just add `-u username` and `-p password` via commandline.
