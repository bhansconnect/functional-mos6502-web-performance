async function roc_effectful_gen_func() {
  let exit_code;
  let wasm_exports;
  let buffer;
  let count;

  const importObj = {
    wasi_snapshot_preview1: {
      proc_exit: (code) => {
        if (code !== 0) {
          console.error(`Exited with code ${code}`);
        }
        exit_code = code;
      },
      fd_write: (_) => { console.error("We don't deal with fd_write"); },
    },
    env: {
      set_output_count: (cnt) => { count = cnt; },
      roc_fx_readMem: (i) => { return buffer[i]; },
      roc_fx_writeMem: (i, val) => { buffer[i] = val; },
      roc_panic: (pointer, _tag_id) => {
        const bytes = new Uint8Array(wasm_exports.memory.buffer, pointer);
        const end = bytes.findIndex(x => x === 0);
        const decoder = new TextDecoder();
        const msg = decoder.decode(bytes.slice(0, end));

        throw `Roc panicked with message: '${msg}'`;
      },
    },
  };

  let wasm;

  const request = fetch("./implementations/roc-effectful/emulator.wasm");

  if (WebAssembly.instantiateStreaming) {
    // streaming API has better performance if available
    wasm = await WebAssembly.instantiateStreaming(request, importObj);
  } else {
    const response = await request;
    const module_bytes = await response.arrayBuffer();
    wasm = await WebAssembly.instantiate(module_bytes, importObj);
  }

  wasm_exports = wasm.instance.exports;

  return function (buf) {
    buffer = new Uint8Array(buf);
    try {
      wasm_exports._start();
    } catch (e) {
      const is_ok = e.message === "unreachable" && exit_code === 0;
      if (!is_ok) {
        console.error(e);
      }
    }
    return count;
  }
}

if (typeof module !== "undefined") {
  module.exports = {
    run,
  };
}
