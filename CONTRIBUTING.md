# Code formating #
## Beautifier ##
* CTRL + D (Width = 80; Tabs to spaces = 2)
It helps to check syntax too..

## For / While / Repeat ##
`Repeat` esentially is different from `For` and `While`, if it's required at least one iterarion use it.

Between `For` and `While`... maybe `While` in most cases.
* `For` is better when we need to skip values inside iteration with `Continue`, making less anidated conditionals and clearer code.
* `While` is better when aborting whole iteration and it's a must when removing items from a list (when deleting an item, the counter value must not be incremented... unless it goes backwards...).
