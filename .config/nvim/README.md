## Layout

### `init.lua`
The entrypoint, which was adapted from [cajus-nvim](https://github.com/rafaeldelboni/cajus-nvim).

This loads `fnl/config/init.fnl` to get things started

### `fnl/config/init.fnl`

has the base vim config.

### `fnl/config/plugin.fnl`

Uses packer to install packages. Those that have a `:mod` defined will load additional configuration files from `fnl/config/plugin/the_plugin_name.clj`.
Plugins with a `:config` function require `:PackerCompile` to be run for modifications to be registered.

### `fnl/config/mapping.fnl`

Contains remappings


## Usage

Now you can use Conjure's Fennel repl to eval configuration forms without needed to restart nvim, source whole config files, or remember vimscript.
