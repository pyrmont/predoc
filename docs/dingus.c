#include <emscripten.h>
#include "janet.h"

static JanetTable *env = NULL;

EMSCRIPTEN_KEEPALIVE
const char *run_janet(char *source) {
    Janet result;
    int ret;

    if (env == NULL) {
        // Initialize the virtual machine
        janet_init();

        // Get the core environment
        env = janet_core_env(NULL);

        // Import predoc
        ret = janet_dostring(env, "(import ./predoc)", "dingus.c", &result);
        if (ret) return NULL;

        // Define convert function
        char *code =
        "(defn convert [name input]\n"
        "  (def nl (string/from-bytes 10))\n"
        "  (def dq (string/from-bytes 34))\n"
        "  (def bs (string/from-bytes 92))\n"
        "  (def escaped (->> input (string/replace-all `\\n` nl)\n"
        "                          (string/replace-all `\\\"` dq)\n"
        "                          (string/replace-all `\\\\` bs)))\n"
        "  (def html (predoc/predoc->html name escaped :no-ad? true)))";
        ret = janet_dostring(env, code, "dingus.c", &result);
        if (ret) return NULL;
    }

    ret = janet_dostring(env, source, "dingus.c", &result);
    if (ret) return NULL;

    return janet_getcstring(&result, 0);
}
