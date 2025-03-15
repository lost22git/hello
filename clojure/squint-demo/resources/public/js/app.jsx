import * as squint_core from 'squint-cljs/core.js';
import * as rdom from 'react-dom/client';
import { useState } from 'react';
var fetch_cat = async function () {
return fetch("https://api.thecatapi.com/v1/images/search", ({ "method": "get" }))
};
var inc_count = async function (setState) {
const res1 = (await fetch_cat());
const data2 = (await res1.json());
return setState((function (_PERCENT_1) {
return squint_core.update(_PERCENT_1, "cats", squint_core.into, data2)
}))
};
var dec_count = function (setState) {
return setState((function (_PERCENT_1) {
return squint_core.update(_PERCENT_1, "cats", squint_core.pop)
}))
};
var UI = function () {
const vec__14 = useState(({ "cats": [] }));
const state5 = squint_core.nth(vec__14, 0, null);
const setState6 = squint_core.nth(vec__14, 1, null);
const cat_list7 = squint_core.get(state5, "cats");
const cat_count8 = squint_core.count(cat_list7);
return <center><h2>Hello Squint</h2><div><button onClick={(function () {
return inc_count(setState6)
})}>+</button><span>{cat_count8}</span><button disabled={(cat_count8 === 0)} onClick={(function () {
return dec_count(setState6)
})}>-</button></div><div>{squint_core.lazy(function* () {
for (let G__9 of squint_core.iterable(cat_list7)) {
const map__1011 = G__9;
const url12 = squint_core.get(map__1011, "url");
yield <img src={url12} width={400} height={400}></img>
}
})}</div></center>
};
var root = rdom.createRoot(document.getElementById("app"));
root.render(<UI></UI>);

export { fetch_cat, inc_count, dec_count, UI, root }
