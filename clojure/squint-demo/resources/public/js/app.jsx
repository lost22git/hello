var squint_core = await import('squint-cljs/core.js');
globalThis.app = globalThis.app || {};
globalThis.app = globalThis.app || {};
var rdom = await import('react-dom/client');
var { useState } = (await import ('react'));
globalThis.app.useState = useState;
globalThis.app.rdom = rdom;
var fetch_cat = async function () {
return fetch("https://api.thecatapi.com/v1/images/search", ({ "method": "get" }))
};
globalThis.app.fetch_cat = fetch_cat;
var inc_count = async function (setState) {
const res1 = (await globalThis.app.fetch_cat());
const data2 = (await res1.json());
return setState((function (_PERCENT_1) {
return squint_core.update(_PERCENT_1, "cats", squint_core.into, data2)
}))
};
globalThis.app.inc_count = inc_count;
var dec_count = function (setState) {
return setState((function (_PERCENT_1) {
return squint_core.update(_PERCENT_1, "cats", squint_core.pop)
}))
};
globalThis.app.dec_count = dec_count;
var ui = function () {
const vec__14 = globalThis.app.useState(({ "cats": [] }));
const state5 = squint_core.nth(vec__14, 0, null);
const setState6 = squint_core.nth(vec__14, 1, null);
const cat_list7 = squint_core.get(state5, "cats");
const cat_count8 = squint_core.count(cat_list7);
return <center><h2>Hello Squint</h2><div><button onClick={(function () {
return globalThis.app.inc_count(setState6)
})}>+</button><span>{cat_count8}</span><button disabled={(cat_count8 === 0)} onClick={(function () {
return globalThis.app.dec_count(setState6)
})}>-</button></div><div>{squint_core.lazy(function* () {
for (let G__9 of squint_core.iterable(cat_list7)) {
const map__1011 = G__9;
const url12 = squint_core.get(map__1011, "url");
yield <img src={url12} width={400} height={400}></img>
}
})}</div></center>
};
globalThis.app.ui = ui;
if ((typeof app !== 'undefined') && (typeof app.root !== 'undefined')) {
} else {
var root = rdom.createRoot(document.getElementById("app"));
globalThis.app.root = root;
};
globalThis.app.root.render(<globalThis.app.ui></globalThis.app.ui>);

export { fetch_cat, inc_count, dec_count, ui, root }
