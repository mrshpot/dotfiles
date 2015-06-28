txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtrst='\e[0m'    # Text Reset

export PS1="${txtgrn}\u@\h ${txtylw}\w\n${txtrst}\$MSYSTEM \\$ "

for prog in msysgit python cmake ; do
    if [ -d "/$prog" ] ; then
        export PATH="$PATH:/$prog/bin"
    fi
done
