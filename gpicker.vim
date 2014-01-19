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
" Release Date: May 15, 2012
"      Version: 0.5
"     Keywords: autocompletion
"
"      Install: Copy file into ~/.vim/plugin directory or put in .vimrc
"
"                 source /path/to/this/script/gpicker.vim
"
"        Usage: To launch the gpicker:
"
"                 <Leader>mr - Opens the gpicker from directory of file.
"                 <Leader>mg - Opens the gpicker from current directory.
"                 <Leader>mf - The same as above, but don't guess SCM.
"                 <Leader>mb - Opens the gpicker to chose from list of
"                              current buffers.
"                 <Leader>mm - Opens the gpicker feeding entries from
"                              mlocate database (use g:gpicker_mlocate_db to
"                              choose one, by default: "/var/lib/mlocate/mlocate.db")
"
"               You can also use the commands correspondingly:
"
"                 ":GPickFileFromHere"
"                 ":GPickFile"
"                 ":GPickFileDefault"
"                 ":GPickBuffer"
"                 ":GPickLocate"
"
"               To generate "mlocate.db" file in current directory you can
"               use ":GGenLocateDB" command.
"

" Exit quickly when already loaded.
if exists("g:loaded_gpicker") || executable("gpicker") == 0
  finish
endif

command GPickFile :call <SID>GPickFile("edit", "", "guess")
command GPickFileDefault :call <SID>GPickFile("edit", "", "default")
command GPickFileFromHere :call <SID>GPickFile("edit", expand("%:h"), "default")
command GPickLocate :call <SID>GPickFile("edit", "", "mlocate")
command GGenLocateDB :call system("updatedb -U " . getcwd() . " -o mlocate.db")
function! s:GPickFile(cmd, path, type)
  if empty(a:path)
    let l:path = getcwd()
  else
    let l:path = a:path
  endif
  " select file via gpicker
  if a:type == "mlocate"
    let l:path = getcwd() . '/mlocate.db'
    if filereadable(l:path) == 0
      if exists("g:gpicker_mlocate_db")
        let l:path = g:gpicker_mlocate_db
      else
        let l:path = "/var/lib/mlocate/mlocate.db"
      endif
    endif
    let l:filename = system('gpicker --eat-prefix="" -t mlocate ' . shellescape(l:path))
  else
    let l:filename = l:path . "/" . system('gpicker -t ' . a:type . " " . shellescape(l:path))
  endif
  if filereadable(l:filename)
    " open selected file
    execute a:cmd . " " . escape(resolve(expand(l:filename)), ' ')
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

nmap <silent> <leader>mm :GPickLocate<cr>
nmap <silent> <leader>mg :GPickFile<cr>
nmap <silent> <leader>mf :GPickFileDefault<cr>
nmap <silent> <leader>mr :GPickFileFromHere<cr>
nmap <silent> <leader>mb :GPickBuffer<cr>

let g:loaded_gpicker = 1
