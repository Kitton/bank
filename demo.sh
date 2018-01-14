
usage() {
    printf "Bank Demo Launcher\nUsage: $SELF (compile|launch)\n"
    exit 1
}

case "${1:-''}" in
    'compile')
        echo "Compiling bank... "
        cd bank/
        rebar3 release
        cd ..
        echo "Compiling assistant... "
        cd assistant/
        mix deps.get
        mix compile
        cd ..
        ;;
    'bank_a')
        echo "Launching Bank A"
        ./bank/_build/default/rel/bank/bin/bank console -config $(pwd)/bank/priv/Bank_A.config -sname bank_a
        ;;
    'bank_b')
        echo "Launching Bank B"
        ./bank/_build/default/rel/bank/bin/bank console -config $(pwd)/bank/priv/Bank_B.config -sname bank_b
        ;;
    'assistant')
        echo "Launching assistant"
        cd assistant/
        iex -S mix phoenix.server
        ;;
    'stop')
        echo "Stopping Bank A"
        ./bank/_build/default/rel/bank/bin/bank stop -sname bank_a
        echo "Stopping Bank B"
        ./bank/_build/default/rel/bank/bin/bank stop -sname bank_b
        ;;
    *)
        usage
        ;;
esac
