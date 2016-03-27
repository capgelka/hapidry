# hapidry

hapidry (haskell api diary) is a command line client for diary.ru api with interface in unix spirit. Feel free to open issues if something is done some other ugly way.

Currently it supports 2 actions.

* Create new post in your blog
	* hapidry post -m message -t subject
	* hapidry post -f '~/prepared_post.txt' -t subject
	* cat prepared_post.md | markdown | hapidry post -p -t subject
	* hapidry post -t subject (write post in your favorite $EDITOR)
* Send new U-mail (private message) to user
    * hapidry send someone -f '~/prepared_umail.txt' -t subject
	* hapidry send someone -m message -t subject
	* cat prepared_post.md | markdown | hapidry send someone -p -t subject
	* hapidry send someone -t subject (write message in your favorite $EDITOR)
* get notifications for umails, discussions and comments. There also an example bash script in examples folder to wrap it, but it's better to use cron for this.
    * hapidry notify

* There are also some additional features
    * hapidry post -m message --tags "tagone, tag2,othertag" (creates new post with 3 tags)
    * hapidry post blog1 blog2 -m message (add new post in 2 blogs with given names!)
    * hapidry send user1 user2 -m message (send message to 2 users)

To start usage you should create config file. The default path is ~/.hapidry, but you can choose another via -c option.

Config has simple key/value format

```
password = "password"
username = "diaryusername"
```

If you need to make actions from different user for one time, don't edit config, just add -u username -p password via commandline.
