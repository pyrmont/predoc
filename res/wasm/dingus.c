#include <emscripten.h>
#include "janet.h"
#include "predoc-image.h"

static JanetTable *env = NULL;

EMSCRIPTEN_KEEPALIVE
const char *run_janet(char *source) {
    Janet result;
    int ret;

    if (env == NULL) {
        Janet lookup = janet_wrap_nil();
        Janet predoc;

        janet_init();
        env = janet_core_env(NULL);

        janet_resolve(env, janet_csymbol("load-image-dict"), &lookup);
        if (!janet_checktype(lookup, JANET_TABLE)) return NULL;

        predoc = janet_unmarshal(
            predoc_image,
            predoc_image_len,
            0,
            janet_unwrap_table(lookup),
            NULL);
        if (!janet_checktype(predoc, JANET_TABLE)) return NULL;

        janet_def(env, "__predoc_image", predoc, NULL);
        ret = janet_dostring(
            env,
            "(merge-module (curenv) __predoc_image \"predoc/\")",
            "dingus.c",
            &result);
        if (ret) return NULL;

        const char *code =
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
