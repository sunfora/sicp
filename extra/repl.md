# NVIM + Racket Repl

До этого я выполнял домашки через nvim + lf + DrRacket. 

И это всё прекрасно работало (в том числе для домашек с картинками).
Но было это и немножко неудобно: постоянно переключать окна и всё такое.

## Проблемы с отображением при использовании #lang r5s5, #lang sicp

repl для racket идёт вместе собственно с racket
и одна из проблем которая тут же вылезает это вот такая дичь:

```
C:\>racket -I sicp
Welcome to Racket v8.11.1 [cs].
> (list 1 2 3 4)
(mcons 1 (mcons 2 (mcons 3 (mcons 4 '()))))
>
```

это читать невозможно
поэтому бежим гуглить, узнаем много нового о том как это настроить

шаги следующие:
1. найти где racket устанавливает пакеты
   для этого исполняем
   ```
   > racket
   Welcome to Racket v8.11.1 [cs].
   > (require setup/dirs)
   > (find-user-pkgs-dir)
   ```
2. переходим по пути и заходим в файл ```./sicp/sicp/main.rkt```
   это по-сути файл который даёт определения языку

   ничего особо не трогаем и добавляем просто 3 строчки в конце:
   ```
   ;; FIX racket repl from displaying (mcons (mcons ))
   (print-as-expression #f)
   (print-pair-curly-braces  #t)
   (print-mpair-curly-braces #f)
   ```

и наслаждаемся тем, что теперь всё красиво отображается в repl:
```
Welcome to Racket v8.11.1 [cs].
> (list 1 2 3 4)
(1 2 3 4)
> (cons 1 2)
(1 . 2)
>
```

## Добавляем шорткаты для взаимодействия с repl

Есть уже готовые разные плагины вроде slimv, vim-slime, reply и т.д.
Вы можете поковыряться с этими вариантами.

Но я пойду немножко другим путём, потому что я не хочу учить ничего.
Поэтому минимальное взаимодействие с repl напишем сами.

Самое простое, что хочется, это научиться отправлять скопированный текст и вообще произвольный текст в repl.

Для этого мы будем использовать встроенную функцию ```chansend```.

Идём в окно с терминалом и пишем: 
```echo b:terminal_job_id```, получаем например ```3```

Теперь откуда угодно мы можем вызвать функцию ```chansend```.
И отправить немножко вещей прямо в терминал:
```call chansend(3, "Hello World")```

Осталось это только всё автоматизировать.

Нам нужно следующее поведение:
1. функция bind, которая будет привзяывать терминал к функции send
2. функция send, которая будет отправлять текст по привязанному channel_id

## Пишем маленький lua модуль

```lua
if (_G.sniff) then
  error("Sniff: global name clash")  
end

_G.sniff = {}

local newline = vim.fn.has("win32") and "\r\n" 
                                    or  "\n"

function sniff:terminal()
  if (not vim.b.terminal_job_id) then
    vim.api.nvim_err_writeln("Sniff: not a terminal")
    return false
  end
  self.job = vim.b.terminal_job_id
  vim.cmd([[echo "Sniff: connected ]] .. self.job .. '"')
  return true
end

function sniff:send(text)
  if (not self.job) then
    vim.api.nvim_err_writeln("Sniff: no job selected")
    return false 
  end
  vim.fn.chansend(self.job, text)
  return true
end

function sniff:sendln(text)
  sniff:send(text .. newline)
end
```

и кладём его в папку plugin, рядом с init.vim

после чего мы теперь сможем вызывать 
```:lua sniff::terminal()```
```:lua snif::send("Hello world\r\n")```

## Добавляем удобные маппинги

идём в init.vim
и заполняем так как мы хотим 

```vimscript
" move between terminal and window 
" via \[ and \]
map <C-\><C-]> <C-w>li
tmap <C-\><C-[> <C-\><C-n><C-w>h

" bind terminal
nmap \. :lua sniff:terminal()<CR>

" send yanked text
nmap \p :lua sniff:send(vim.fn.getreg('"'))<CR>

" send newline
nmap \<CR> :lua sniff:sendln("")<CR>

" send yanked text and newline
nmap \; \p\<CR>

" send quote 
nmap \' :lua sniff:send("'")<CR>

" send opening bracket 
nmap \9 :lua sniff:send("(")<CR>
" send closing bracket 
nmap \0 :lua sniff:send(")")<CR>

" yank current list and send it
nmap \( ya(\;

" enter file
nmap \> :lua sniff:sendln(',enter (file "'..string.gsub(vim.fn.expand("%"), [[\]], [[\\]])..'")')<CR><CR>

" go upper
nmap \< :lua sniff:sendln(",top")
```

я например поставил следующие:
Cntrl-\ Cntrl-] --> переход в insert mode repl
Cntrl-\ Cntrl-[ --> переход обратно в левое окно

в теории то, что выше можно круче сделать, но я не стал заморачиваться

\p я забиндил на отправить текст из copy-регистра
то есть теперь я могу что-то скопировать и отправить напрямую в repl

\Enter я забиндил на отправку новой строки

\( на отправку целого списка

и самое интересное
\> на ,enter (file filename)

чтобы с лёгкостью исполнять файлы над которыми работаю
