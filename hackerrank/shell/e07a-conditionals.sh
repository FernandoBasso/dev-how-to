read -r answer

case "$answer" in
  [Yy]*)
    echo YES
    ;;
  [Nn]*)
    echo NO
    ;;
  *)
    echo 'Crap 💩'
    ;;
esac

