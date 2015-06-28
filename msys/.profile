export PS1='\033[32m\]\u@\h \[\033[33m\w\033[0m\]
$MSYSTEM \$ '

for prog in msysgit python cmake ; do
    if [ -d "/$prog" ] ; then
        export PATH="$PATH:/$prog/bin"
    fi
done
