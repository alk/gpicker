"    Copyright: This program is free software: you can redistribute it and/or 
"               modify it under the terms of the GNU General Public License as
"               published by the Free Software Foundation; either version 3 of the
"               License, or (at your option) any later version.
"
"               This program is distributed in the hope that it will be useful,
"               but WITHOUT ANY WARRANTY; without even the implied warranty of
"               MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
"               General Public License for more details.
"
"               You should have received a copy of the GNU General Public License
"               along with this program.  If not, see
"               `http://www.gnu.org/licenses/'.
"
" Name Of File: gpicker.vim
"  Description: Bindings for gpicker, fast file chooser.
"  Maintainers: Sergey Avseyev <sergey.avseyev@gmail.com>
" Contributors:
"
" Release Date: October 20, 2009
"      Version: 0.1
"     Keywords: autocompletion
"
"      Install: Copy file into ~/.vim/plugin directory or put in .vimrc
"
"                 source /path/to/this/script/gpicker.vim
"
"        Usage: To launch the gpicker:
"
"                 <Leader>g  - Opens the gpicker from current directory.
"
"               You can also use the command:
"
"                 ":Gpicker"
"

" Exit quickly when already loaded.
if exists("g:loaded_gpicker")
  finish
endif

command Gpicker :call <SID>Gpicker()
function! s:Gpicker()
  let filename = substitute(system('gpicker .'), '\n\+', '', 'g')
  if filereadable(filename)
    execute "edit " . filename
  endif
endfunction

nmap <silent> <leader>g :Gpicker<cr>

" vim:sw=2:sts=2:et:
