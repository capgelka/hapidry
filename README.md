# hapidry

hapidry is a command line client for diary.ru apiтwith interface in unix spirit. Feel free to open issues if something is done some other ugly way.

Currently it supports 2 actions.

* Create new post in yout blog
	* hapidry post -m message -t subject
	* hapidry post -f '~/prepared_post.txt' -t subject
	* cat prepared_post.md | markdown | hapidry post -p -t subject
	* hapidry post -t subject (write post in your favorite $EDITOR)
* Send new U-mail (private message) to user
    * hapidry send someone -f '~/prepared_umail.txt' -t subject
	* hapidry send someone -m message -t subject
	* cat prepared_post.md | markdown | hapidry send someone -p -t subject
	* hapidry send someone -t subject (write message in your favorite $EDITOR)

To start usage you should create config file, the default path is ~/.hapidry, bu you can choose another via -c option.

Config has simple key/value format

```
password = "password"
username = "diaryusername"
```

If you need to make actions from different user for one time, don't edit config, just add -u username -p password via commandline.
