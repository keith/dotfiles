" Vim syntax file
" Language:    Pentadactyl configuration file
" Maintainer:  Doug Kearns <dougkearns@gmail.com>
" Version:     hg7127

if exists("b:current_syntax")
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

syn include @javascriptTop syntax/javascript.vim
unlet b:current_syntax

syn include @cssTop syntax/css.vim
unlet b:current_syntax

syn match pentadactylCommandStart "\%(^\s*:\=\)\@<=" nextgroup=pentadactylCommand,pentadactylAutoCmd

syn keyword pentadactylCommand loadplugins lpl gr[oup] ! run Clistk[eys] Clk
    \ Cm[ap] Cno[remap] Cunm[ap] Ilistk[eys] Ilk Im[ap] Ino[remap] Iunm[ap]
    \ ab[breviate] addo[ns] ao au[tocmd] ba[ck] background bg bd[elete]
    \ blistk[eys] blk bm[ap] bma[rk] bmarks bno[remap] b[uffer] buffers files ls
    \ tabs bunm[ap] ca[bbreviate] caretlistk[eys] caretlk caretm[ap]
    \ caretno[remap] caretunm[ap] cd chd[ir] clistk[eys] clk cm[ap] cno[remap]
    \ colo[rscheme] com[mand] comp[letions] contexts cookies ck cuna[bbreviate]
    \ cunm[ap] delbm[arks] delc[ommand] delg[roup] delmac[ros] delm[arks]
    \ delqm[arks] dels[tyle] dia[log] dlc[lear] doautoa[ll] do[autocmd]
    \ downl[oads] dl ec[ho] echoe[rr] echom[sg] el[se] elsei[f] elif em[enu]
    \ en[dif] fi exe[cute] exit x exta[dd] extde[lete] extrm extd[isable]
    \ exte[nable] exto[ptions] extp[references] extr[ehash] extt[oggle]
    \ extu[pdate] feedkeys fk fini[sh] fo[rward] fw frameo[nly] ha[rdcopy] h[elp],halp
    \ helpa[ll] hi[ghlight] hist[ory] hs ia[bbreviate] if ilistk[eys] ilk im[ap]
    \ ino[remap] iuna[bbreviate] iunm[ap] javas[cript] js ju[mps] keepa[lt] let
    \ listc[ommands] lc listk[eys] lk listo[ptions] lo mac[ros] map ma[rk] marks
    \ mes[sages] messc[lear] mkp[entadactylrc] mkv[imruntime] mlistk[eys] mlk
    \ mm[ap] mno[remap] munm[ap] nlistk[eys] nlk nm[ap] nno[remap] noh[lfind]
    \ no[remap] norm[al] nunm[ap] olistk[eys] olk om[ap] ono[remap] o[pen]
    \ ounm[ap] pa[geinfo] pagest[yle] pas pin[tab] pref[erences] prefs pr[ivate]
    \ pr0n porn pw[d] qma[rk] qmarks q[uit] quita[ll] qa[ll] redr[aw]
    \ reg[isters] reh[ash] re[load] reloada[ll] res[tart] runt[ime] sa[nitize]
    \ sav[eas] w[rite] sbcl[ose] scrip[tnames] se[t] setg[lobal] setl[ocal]
    \ sideb[ar] sb[ar] sbop[en] sil[ent] so[urce] st[op] stopa[ll] sty[le]
    \ styled[isable] styd[isable] stylee[nable] stye[nable] stylet[oggle]
    \ styt[oggle] tab taba[ttach] tabc[lose] tabde[tach] tabd[o] bufd[o]
    \ tabdu[plicate] tabl[ast] bl[ast] tabm[ove] tabn[ext] tn[ext] bn[ext]
    \ tabo[nly] tabopen t[open] tabnew tabp[revious] tp[revious] tabN[ext]
    \ tN[ext] bp[revious] bN[ext] tabr[ewind] tabfir[st] br[ewind] bf[irst] time
    \ tlistk[eys] tlk tm[ap] tno[remap] toolbarh[ide] tbh[ide] toolbars[how]
    \ tbs[how] toolbart[oggle] tbt[oggle] tunm[ap] una[bbreviate] u[ndo]
    \ undoa[ll] unl[et] unm[ap] unpin[tab] verb[ose] ve[rsion] vie[wsource]
    \ vlistk[eys] vlk vm[ap] vno[remap] vunm[ap] winc[lose] wc[lose] wind[ow]
    \ winon[ly] wino[pen] wo[pen] wqa[ll] wq xa[ll] y[ank] zo[om]
    \ contained

syn match pentadactylCommand "!" contained

syn keyword pentadactylAutoCmd au[tocmd] contained nextgroup=pentadactylAutoEventList skipwhite

syn keyword pentadactylAutoEvent BookmarkAdd BookmarkChange BookmarkRemove
    \ ColorScheme DOMLoad DownloadPost Fullscreen LocationChange PageLoadPre
    \ PageLoad PrivateMode Sanitize ShellCmdPost Enter LeavePre Leave
    \ contained

syn match pentadactylAutoEventList "\(\a\+,\)*\a\+" contained contains=pentadactylAutoEvent

syn region pentadactylSet matchgroup=pentadactylCommand start="\%(^\s*:\=\)\@<=\<\%(setl\%[ocal]\|setg\%[lobal]\|set\=\)\=\>"
    \ end="$" keepend oneline contains=pentadactylOption,pentadactylString

syn keyword pentadactylOption activate act altwildmode awim autocomplete au
    \ cdpath cd complete cpt cookieaccept ca cookielifetime cl cookies ck
    \ defsearch ds downloadsort dlsort dls editor encoding enc eventignore ei
    \ extendedhinttags eht fileencoding fenc findcase fc findflags ff
    \ followhints fh guioptions go helpfile hf hintinputs hin hintkeys hk
    \ hintmatching hm hinttags ht hinttimeout hto history hi iskeyword isk
    \ jumptags jt linenumbers ln loadplugins lpl maxitems messages msgs newtab
    \ nextpattern pageinfo pa passkeys pk passunknown pu popups pps
    \ previouspattern runtimepath rtp sanitizeitems si sanitizeshutdown ss
    \ sanitizetimespan sts scroll scr scrollsteps scs scrolltime sct shell sh
    \ shellcmdflag shcf showmode smd showstatuslinks ssli showtabline stal
    \ spelllang spl strictfocus sf suggestengines timeoutlen tmol titlestring
    \ urlseparator urlsep us verbose vbs wildanchor wia wildcase wic wildignore
    \ wig wildmode wim wildsort wis wordseparators wsp yankshort ys
    \ contained nextgroup=pentadactylSetMod

let s:toggleOptions = ["banghist", "bh", "errorbells", "eb", "exrc", "ex",
    \ "fullscreen", "fs", "hlfind", "hlf", "incfind", "if", "insertmode", "im",
    \ "more", "online", "timeout", "tmo", "usermode", "um", "visualbell", "vb"]
execute 'syn match pentadactylOption "\<\%(no\|inv\)\=\%(' .
    \ join(s:toggleOptions, '\|') .
    \ '\)\>!\=" contained nextgroup=pentadactylSetMod'

syn match pentadactylSetMod "\%(\<[a-z_]\+\)\@<=&" contained

syn region pentadactylJavaScript start="\%(^\s*\%(javascript\|js\)\s\+\)\@<=" end="$" contains=@javascriptTop keepend oneline
syn region pentadactylJavaScript matchgroup=pentadactylJavaScriptDelimiter
    \ start="\%(^\s*\%(javascript\|js\)\s\+\)\@<=<<\s*\z(\h\w*\)"hs=s+2 end="^\z1$" contains=@javascriptTop fold

let s:cssRegionStart = '\%(^\s*sty\%[le]!\=\s\+\%(-\%(n\|name\)\%(\s\+\|=\)\S\+\s\+\)\=[^-]\S\+\s\+\)\@<='
execute 'syn region pentadactylCss start="' . s:cssRegionStart . '" end="$" contains=@cssTop keepend oneline'
execute 'syn region pentadactylCss matchgroup=pentadactylCssDelimiter'
    \ 'start="' . s:cssRegionStart . '<<\s*\z(\h\w*\)"hs=s+2 end="^\z1$" contains=@cssTop fold'

syn match pentadactylNotation "<[0-9A-Za-z-]\+>"

syn keyword pentadactylTodo FIXME NOTE TODO XXX contained

syn region pentadactylString start="\z(["']\)" end="\z1" skip="\\\\\|\\\z1" oneline

syn match pentadactylComment +^\s*".*$+ contains=pentadactylTodo,@Spell

" NOTE: match vim.vim highlighting group names
hi def link pentadactylAutoCmd               pentadactylCommand
hi def link pentadactylAutoEvent             Type
hi def link pentadactylCommand               Statement
hi def link pentadactylJavaScriptDelimiter   Delimiter
hi def link pentadactylCssDelimiter          Delimiter
hi def link pentadactylNotation              Special
hi def link pentadactylComment               Comment
hi def link pentadactylOption                PreProc
hi def link pentadactylSetMod                pentadactylOption
hi def link pentadactylString                String
hi def link pentadactylTodo                  Todo

let b:current_syntax = "pentadactyl"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: tw=130 et ts=8 sts=4 sw=4:
