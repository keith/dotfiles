require("telescope").setup {
  defaults = {
    layout_config = {
      height = 0.95,
      width = 0.95,
      preview_cutoff = 20,
      preview_height = 0.65,
    },
    layout_strategy = "vertical",
    mappings = {
      i = {
        ["<C-s>"] = require("telescope.actions").select_horizontal,
        ["<C-x>"] = nil,
      },
    },
  },
  extensions = {
    fzy_native = {
      override_generic_sorter = true,
      override_file_sorter = true,
    },
  },
}

require("telescope").load_extension "fzy_native"
