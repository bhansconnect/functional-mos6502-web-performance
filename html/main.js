let implementations = {};

async function measure(label, act) {
    const maxRuns = 100;
    const minTimeMS = 10;

    let buffers = [];
    for (let i = 0; i < maxRuns; ++i) {
        const buf = files["data/program.dat"].slice();
        buffers.push(buf);
    }

    // time for at least minTimeMS.
    // the resultion on my browser seems to be 0.1ms which is problematic for roc.
    // We will time at most maxRuns times due to how many buffers are loaded.
    const before = performance.now();
    let runs = 0;
    let cnt = 0;
    while(runs < maxRuns && performance.now() - before < minTimeMS) {
        cnt = await act(buffers[runs]);
        ++runs;
    }
    const after = performance.now();
    if (cnt != 4142) throw { label: label, cnt: cnt };

    const time = (after - before)/runs;
    // console.log(label + ": " + cnt + " cycles done in " + time + "ms");
    return time;
};

async function measureAll() {
    const numRuns = 100;
    const numWarmup = 100;

    document.body.innerHTML = "Running...";
    for (const [label, act] of Object.entries(implementations)) {
        for (let i = 0; i < numWarmup; ++i) {
            await measure(label, act);
        }

        let times = [];
        for (let i = 0; i < numRuns; ++i) {
            if (i % 20 == 0) {
                console.log("Running " + label + "...");
            }
            times.push(await measure(label, act));
        }

        let minTime = null, sumTime = 0, maxTime = null;
        for (const time of times) {
            minTime = (minTime == null) || time < minTime ? time : minTime;
            maxTime = (maxTime == null) || time > maxTime ? time : maxTime;
            sumTime += time;
        }

        const avgTime = sumTime / numRuns;

        console.log(label + ":" +
            " min: " + minTime + "ms" +
            " max: " + maxTime + "ms" +
            " avg: " + avgTime + "ms");
    }
    document.body.innerHTML = "Done!<br/>Look at the JS console for the results.";
}

async function setup() {
    {
        const mod = await import("./implementations/js/mos6502.js");
        implementations["JavaScript"] = async buf => mod.run(buf)();
    }

    implementations["Idris2"] = async buf => idris2_run(buf);

    roc_func = await roc_gen_func();
    implementations["Roc"] = async buf => roc_func(buf);

    roc_effectful_func = await roc_effectful_gen_func();
    implementations["Roc-Effectful"] = async buf => roc_effectful_func(buf);
    // {
    //     const mod = await import("./implementations/purescript/bundle.js");
    //     implementations["PureScript"] = async buf => mod.run(buf)();
    // }

    // {
    //     const mod = await import("../implementations/asterius/_build/Driver.mjs");
    //     const run = await mod.setup();
    //     implementations["GHC-Asterius"] = async buf => await run(buf);
    // }
}

window.addEventListener("load", function () {
    setup().then(measureAll);
});
