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
" Release Date: December 7, 2009
"      Version: 0.2
"     Keywords: autocompletion
"
"      Install: Copy file into ~/.vim/plugin directory or put in .vimrc
"
"                 source /path/to/this/script/gpicker.vim
"
"        Usage: To launch the gpicker:
"
"                 <Leader>lg - Opens the gpicker from current directory.
"                 <Leader>m  - Opens the gpicker to chose from list of current
"                 buffers.
"
"               You can also use the command:
"
"                 ":GPickFile"
"                 ":GPickFileFromHere"
"                 ":GPickBuffer"
"

" Exit quickly when already loaded.
if exists("g:loaded_gpicker") || executable("gpicker") == 0
  finish
endif

command GPickFile :call <SID>GPickFile(".")
command GPickFileFromHere :call <SID>GPickFile(expand("%:h"))
function! s:GPickFile(path)
  if empty(a:path)
    let l:path = "."
  else
    let l:path = a:path
  endif
  " select file via gpicker
  let l:filename = l:path . "/" . system('gpicker -t guess ' . l:path)
  if filereadable(l:filename)
    " open selected file
    execute "edit " . l:filename
  endif
endfunction

command GPickBuffer :call <SID>GPickBuffer()
function! s:GPickBuffer()
  " grab list of buffers
  redir => l:ls_output
  silent execute 'ls'
  redir END

  " remove empty line from beginning and trailing line info
  let l:items = strpart(substitute(l:ls_output, '\(\d\+\)\s\+\([u%#ah=+x-]\+\)\s\+"\(.\{-}\)"\s\{-}line\s\+\d\+', '\3   \2 \1', 'g'), 1)
  " get selection via gpicker
  let l:selected  = system('gpicker --name-separator \\n -', items)
  " open buffer
  execute "buffer " . substitute(l:selected, '[u%#ah=+x-]\+\s\+\d\+$', '', '')
endfunction

command GPickRiDoc :call <SID>GPickRiDoc()
function! s:GPickRiDoc()
  let l:cursor = expand("<cword>")
  " get selection via gpicker
  let l:selected  = system('fasteri | gpicker --init-filter="'. l:cursor .'" --name-separator \\n -')

  if empty(l:selected) == 0
    " open buffer
    execute "new [ri]"
    setlocal buftype=nofile readonly modifiable
    setlocal bufhidden=wipe
    let l:contents = system(printf("fasteri '%s'", l:selected))
    silent put=l:contents
    keepjumps 0d
    setlocal nomodifiable
  endif
endfunction

nmap <silent> <leader>lg :GPickFile<cr>
nmap <silent> <leader>lr :GPickFileFromHere<cr>
nmap <silent> <leader>m :GPickBuffer<cr>
nmap <silent> <leader>k :GPickRiDoc<cr>

let g:loaded_gpicker = 1
