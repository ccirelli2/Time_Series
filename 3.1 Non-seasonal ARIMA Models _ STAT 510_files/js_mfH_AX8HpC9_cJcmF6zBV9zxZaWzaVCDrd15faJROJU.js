/* Source and licensing information for the line(s) below can be found at https://online.stat.psu.edu/stat510/modules/mathjax/js/setup.js. */
(function($,Drupal,document,MathJax){'use strict';Drupal.behaviors.mathjax={attach:function(context,settings){$(document).ajaxComplete(function(){MathJax.Hub.Queue(['Typeset',MathJax.Hub])});if(settings.mathjax.config_type===0)$('body').addClass('tex2jax_ignore')}}}(jQuery,Drupal,document,MathJax))
/* Source and licensing information for the above line(s) can be found at https://online.stat.psu.edu/stat510/modules/mathjax/js/setup.js. */;
/* Source and licensing information for the line(s) below can be found at https://online.stat.psu.edu/stat510/themes/onlinecourses_theme/dist/js/main.bundle.js. */
!function(t){var e={};function n(r){if(e[r])return e[r].exports;var o=e[r]={i:r,l:!1,exports:{}};return t[r].call(o.exports,o,o.exports,n),o.l=!0,o.exports}n.m=t,n.c=e,n.d=function(t,e,r){n.o(t,e)||Object.defineProperty(t,e,{enumerable:!0,get:r})},n.r=function(t){"undefined"!=typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(t,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(t,"__esModule",{value:!0})},n.t=function(t,e){if(1&e&&(t=n(t)),8&e)return t;if(4&e&&"object"==typeof t&&t&&t.__esModule)return t;var r=Object.create(null);if(n.r(r),Object.defineProperty(r,"default",{enumerable:!0,value:t}),2&e&&"string"!=typeof t)for(var o in t)n.d(r,o,function(e){return t[e]}.bind(null,o));return r},n.n=function(t){var e=t&&t.__esModule?function(){return t.default}:function(){return t};return n.d(e,"a",e),e},n.o=function(t,e){return Object.prototype.hasOwnProperty.call(t,e)},n.p="",n(n.s=105)}([function(t,e,n){(function(e){var n=function(t){return t&&t.Math==Math&&t};t.exports=n("object"==typeof globalThis&&globalThis)||n("object"==typeof window&&window)||n("object"==typeof self&&self)||n("object"==typeof e&&e)||Function("return this")()}).call(this,n(59))},function(t,e){t.exports=function(t){try{return!!t()}catch(t){return!0}}},function(t,e){var n={}.hasOwnProperty;t.exports=function(t,e){return n.call(t,e)}},function(t,e,n){var r=n(0),o=n(27),i=n(2),c=n(28),u=n(30),a=n(50),f=o("wks"),s=r.Symbol,l=a?s:s&&s.withoutSetter||c;t.exports=function(t){return i(f,t)||(u&&i(s,t)?f[t]=s[t]:f[t]=l("Symbol."+t)),f[t]}},function(t,e,n){var r=n(7);t.exports=function(t){if(!r(t))throw TypeError(String(t)+" is not an object");return t}},function(t,e,n){var r=n(6),o=n(8),i=n(15);t.exports=r?function(t,e,n){return o.f(t,e,i(1,n))}:function(t,e,n){return t[e]=n,t}},function(t,e,n){var r=n(1);t.exports=!r((function(){return 7!=Object.defineProperty({},1,{get:function(){return 7}})[1]}))},function(t,e){t.exports=function(t){return"object"==typeof t?null!==t:"function"==typeof t}},function(t,e,n){var r=n(6),o=n(33),i=n(4),c=n(20),u=Object.defineProperty;e.f=r?u:function(t,e,n){if(i(t),e=c(e,!0),i(n),o)try{return u(t,e,n)}catch(t){}if("get"in n||"set"in n)throw TypeError("Accessors not supported");return"value"in n&&(t[e]=n.value),t}},function(t,e,n){var r=n(0),o=n(26).f,i=n(5),c=n(13),u=n(21),a=n(44),f=n(63);t.exports=function(t,e){var n,s,l,p,d,v=t.target,h=t.global,m=t.stat;if(n=h?r:m?r[v]||u(v,{}):(r[v]||{}).prototype)for(s in e){if(p=e[s],l=t.noTargetGet?(d=o(n,s))&&d.value:n[s],!f(h?s:v+(m?".":"#")+s,t.forced)&&void 0!==l){if(typeof p==typeof l)continue;a(p,l)}(t.sham||l&&l.sham)&&i(p,"sham",!0),c(n,s,p,t)}}},function(t,e,n){var r=n(32),o=n(11);t.exports=function(t){return r(o(t))}},function(t,e){t.exports=function(t){if(null==t)throw TypeError("Can't call method on "+t);return t}},function(t,e){var n={}.toString;t.exports=function(t){return n.call(t).slice(8,-1)}},function(t,e,n){var r=n(0),o=n(5),i=n(2),c=n(21),u=n(35),a=n(24),f=a.get,s=a.enforce,l=String(String).split("String");(t.exports=function(t,e,n,u){var a=!!u&&!!u.unsafe,f=!!u&&!!u.enumerable,p=!!u&&!!u.noTargetGet;"function"==typeof n&&("string"!=typeof e||i(n,"name")||o(n,"name",e),s(n).source=l.join("string"==typeof e?e:"")),t!==r?(a?!p&&t[e]&&(f=!0):delete t[e],f?t[e]=n:o(t,e,n)):f?t[e]=n:c(e,n)})(Function.prototype,"toString",(function(){return"function"==typeof this&&f(this).source||u(this)}))},function(t,e){var n=Math.ceil,r=Math.floor;t.exports=function(t){return isNaN(t=+t)?0:(t>0?r:n)(t)}},function(t,e){t.exports=function(t,e){return{enumerable:!(1&t),configurable:!(2&t),writable:!(4&t),value:e}}},function(t,e){t.exports={}},function(t,e,n){var r=n(27),o=n(28),i=r("keys");t.exports=function(t){return i[t]||(i[t]=o(t))}},function(t,e,n){var r=n(14),o=Math.min;t.exports=function(t){return t>0?o(r(t),9007199254740991):0}},function(t,e,n){var r=n(11);t.exports=function(t){return Object(r(t))}},function(t,e,n){var r=n(7);t.exports=function(t,e){if(!r(t))return t;var n,o;if(e&&"function"==typeof(n=t.toString)&&!r(o=n.call(t)))return o;if("function"==typeof(n=t.valueOf)&&!r(o=n.call(t)))return o;if(!e&&"function"==typeof(n=t.toString)&&!r(o=n.call(t)))return o;throw TypeError("Can't convert object to primitive value")}},function(t,e,n){var r=n(0),o=n(5);t.exports=function(t,e){try{o(r,t,e)}catch(n){r[t]=e}return e}},function(t,e){t.exports=!1},function(t,e){t.exports=["constructor","hasOwnProperty","isPrototypeOf","propertyIsEnumerable","toLocaleString","toString","valueOf"]},function(t,e,n){var r,o,i,c=n(60),u=n(0),a=n(7),f=n(5),s=n(2),l=n(17),p=n(16),d=u.WeakMap;if(c){var v=new d,h=v.get,m=v.has,y=v.set;r=function(t,e){return y.call(v,t,e),e},o=function(t){return h.call(v,t)||{}},i=function(t){return m.call(v,t)}}else{var g=l("state");p[g]=!0,r=function(t,e){return f(t,g,e),e},o=function(t){return s(t,g)?t[g]:{}},i=function(t){return s(t,g)}}t.exports={set:r,get:o,has:i,enforce:function(t){return i(t)?o(t):r(t,{})},getterFor:function(t){return function(e){var n;if(!a(e)||(n=o(e)).type!==t)throw TypeError("Incompatible receiver, "+t+" required");return n}}}},,function(t,e,n){var r=n(6),o=n(43),i=n(15),c=n(10),u=n(20),a=n(2),f=n(33),s=Object.getOwnPropertyDescriptor;e.f=r?s:function(t,e){if(t=c(t),e=u(e,!0),f)try{return s(t,e)}catch(t){}if(a(t,e))return i(!o.f.call(t,e),t[e])}},function(t,e,n){var r=n(22),o=n(36);(t.exports=function(t,e){return o[t]||(o[t]=void 0!==e?e:{})})("versions",[]).push({version:"3.6.2",mode:r?"pure":"global",copyright:"© 2020 Denis Pushkarev (zloirock.ru)"})},function(t,e){var n=0,r=Math.random();t.exports=function(t){return"Symbol("+String(void 0===t?"":t)+")_"+(++n+r).toString(36)}},function(t,e,n){var r=n(45),o=n(0),i=function(t){return"function"==typeof t?t:void 0};t.exports=function(t,e){return arguments.length<2?i(r[t])||i(o[t]):r[t]&&r[t][e]||o[t]&&o[t][e]}},function(t,e,n){var r=n(1);t.exports=!!Object.getOwnPropertySymbols&&!r((function(){return!String(Symbol())}))},function(t,e,n){"use strict";var r,o,i=n(67),c=n(73),u=RegExp.prototype.exec,a=String.prototype.replace,f=u,s=(r=/a/,o=/b*/g,u.call(r,"a"),u.call(o,"a"),0!==r.lastIndex||0!==o.lastIndex),l=c.UNSUPPORTED_Y||c.BROKEN_CARET,p=void 0!==/()??/.exec("")[1];(s||p||l)&&(f=function(t){var e,n,r,o,c=this,f=l&&c.sticky,d=i.call(c),v=c.source,h=0,m=t;return f&&(-1===(d=d.replace("y","")).indexOf("g")&&(d+="g"),m=String(t).slice(c.lastIndex),c.lastIndex>0&&(!c.multiline||c.multiline&&"\n"!==t[c.lastIndex-1])&&(v="(?: "+v+")",m=" "+m,h++),n=new RegExp("^(?:"+v+")",d)),p&&(n=new RegExp("^"+v+"$(?!\\s)",d)),s&&(e=c.lastIndex),r=u.call(f?n:c,m),f?r?(r.input=r.input.slice(h),r[0]=r[0].slice(h),r.index=c.lastIndex,c.lastIndex+=r[0].length):c.lastIndex=0:s&&r&&(c.lastIndex=c.global?r.index+r[0].length:e),p&&r&&r.length>1&&a.call(r[0],n,(function(){for(o=1;o<arguments.length-2;o++)void 0===arguments[o]&&(r[o]=void 0)})),r}),t.exports=f},function(t,e,n){var r=n(1),o=n(12),i="".split;t.exports=r((function(){return!Object("z").propertyIsEnumerable(0)}))?function(t){return"String"==o(t)?i.call(t,""):Object(t)}:Object},function(t,e,n){var r=n(6),o=n(1),i=n(34);t.exports=!r&&!o((function(){return 7!=Object.defineProperty(i("div"),"a",{get:function(){return 7}}).a}))},function(t,e,n){var r=n(0),o=n(7),i=r.document,c=o(i)&&o(i.createElement);t.exports=function(t){return c?i.createElement(t):{}}},function(t,e,n){var r=n(36),o=Function.toString;"function"!=typeof r.inspectSource&&(r.inspectSource=function(t){return o.call(t)}),t.exports=r.inspectSource},function(t,e,n){var r=n(0),o=n(21),i=r["__core-js_shared__"]||o("__core-js_shared__",{});t.exports=i},function(t,e,n){var r=n(38),o=n(23).concat("length","prototype");e.f=Object.getOwnPropertyNames||function(t){return r(t,o)}},function(t,e,n){var r=n(2),o=n(10),i=n(40).indexOf,c=n(16);t.exports=function(t,e){var n,u=o(t),a=0,f=[];for(n in u)!r(c,n)&&r(u,n)&&f.push(n);for(;e.length>a;)r(u,n=e[a++])&&(~i(f,n)||f.push(n));return f}},function(t,e,n){var r,o=n(4),i=n(65),c=n(23),u=n(16),a=n(66),f=n(34),s=n(17),l=s("IE_PROTO"),p=function(){},d=function(t){return"<script>"+t+"<\/script>"},v=function(){try{r=document.domain&&new ActiveXObject("htmlfile")}catch(t){}var t,e;v=r?function(t){t.write(d("")),t.close();var e=t.parentWindow.Object;return t=null,e}(r):((e=f("iframe")).style.display="none",a.appendChild(e),e.src=String("javascript:"),(t=e.contentWindow.document).open(),t.write(d("document.F=Object")),t.close(),t.F);for(var n=c.length;n--;)delete v.prototype[c[n]];return v()};u[l]=!0,t.exports=Object.create||function(t,e){var n;return null!==t?(p.prototype=o(t),n=new p,p.prototype=null,n[l]=t):n=v(),void 0===e?n:i(n,e)}},function(t,e,n){var r=n(10),o=n(18),i=n(62),c=function(t){return function(e,n,c){var u,a=r(e),f=o(a.length),s=i(c,f);if(t&&n!=n){for(;f>s;)if((u=a[s++])!=u)return!0}else for(;f>s;s++)if((t||s in a)&&a[s]===n)return t||s||0;return!t&&-1}};t.exports={includes:c(!0),indexOf:c(!1)}},function(t,e,n){var r=n(6),o=n(1),i=n(2),c=Object.defineProperty,u=function(t){throw t};t.exports=function(t,e){e||(e={});var n=[][t],a=!!i(e,"ACCESSORS")&&e.ACCESSORS,f=i(e,0)?e[0]:u,s=i(e,1)?e[1]:void 0;return!!n&&!o((function(){if(a&&!r)return!0;var t={length:-1},e=function(e){a?c(t,e,{enumerable:!0,get:u}):t[e]=1};e(1),e(2147483646),e(4294967294),n.call(t,f,s)}))}},,function(t,e,n){"use strict";var r={}.propertyIsEnumerable,o=Object.getOwnPropertyDescriptor,i=o&&!r.call({1:2},1);e.f=i?function(t){var e=o(this,t);return!!e&&e.enumerable}:r},function(t,e,n){var r=n(2),o=n(61),i=n(26),c=n(8);t.exports=function(t,e){for(var n=o(e),u=c.f,a=i.f,f=0;f<n.length;f++){var s=n[f];r(t,s)||u(t,s,a(e,s))}}},function(t,e,n){var r=n(0);t.exports=r},function(t,e){e.f=Object.getOwnPropertySymbols},function(t,e,n){var r=n(48),o=n(32),i=n(19),c=n(18),u=n(64),a=[].push,f=function(t){var e=1==t,n=2==t,f=3==t,s=4==t,l=6==t,p=5==t||l;return function(d,v,h,m){for(var y,g,x=i(d),b=o(x),S=r(v,h,3),w=c(b.length),E=0,O=m||u,j=e?O(d,w):n?O(d,0):void 0;w>E;E++)if((p||E in b)&&(g=S(y=b[E],E,x),t))if(e)j[E]=g;else if(g)switch(t){case 3:return!0;case 5:return y;case 6:return E;case 2:a.call(j,y)}else if(s)return!1;return l?-1:f||s?s:j}};t.exports={forEach:f(0),map:f(1),filter:f(2),some:f(3),every:f(4),find:f(5),findIndex:f(6)}},function(t,e,n){var r=n(54);t.exports=function(t,e,n){if(r(t),void 0===e)return t;switch(n){case 0:return function(){return t.call(e)};case 1:return function(n){return t.call(e,n)};case 2:return function(n,r){return t.call(e,n,r)};case 3:return function(n,r,o){return t.call(e,n,r,o)}}return function(){return t.apply(e,arguments)}}},function(t,e,n){var r=n(12);t.exports=Array.isArray||function(t){return"Array"==r(t)}},function(t,e,n){var r=n(30);t.exports=r&&!Symbol.sham&&"symbol"==typeof Symbol.iterator},function(t,e,n){var r=n(38),o=n(23);t.exports=Object.keys||function(t){return r(t,o)}},function(t,e,n){var r=n(14),o=n(11),i=function(t){return function(e,n){var i,c,u=String(o(e)),a=r(n),f=u.length;return a<0||a>=f?t?"":void 0:(i=u.charCodeAt(a))<55296||i>56319||a+1===f||(c=u.charCodeAt(a+1))<56320||c>57343?t?u.charAt(a):i:t?u.slice(a,a+2):c-56320+(i-55296<<10)+65536}};t.exports={codeAt:i(!1),charAt:i(!0)}},,function(t,e){t.exports=function(t){if("function"!=typeof t)throw TypeError(String(t)+" is not a function");return t}},function(t,e,n){"use strict";var r=n(1);t.exports=function(t,e){var n=[][t];return!!n&&r((function(){n.call(null,e||function(){throw 1},1)}))}},function(t,e,n){"use strict";var r=n(9),o=n(31);r({target:"RegExp",proto:!0,forced:/./.exec!==o},{exec:o})},,,function(t,e){var n;n=function(){return this}();try{n=n||new Function("return this")()}catch(t){"object"==typeof window&&(n=window)}t.exports=n},function(t,e,n){var r=n(0),o=n(35),i=r.WeakMap;t.exports="function"==typeof i&&/native code/.test(o(i))},function(t,e,n){var r=n(29),o=n(37),i=n(46),c=n(4);t.exports=r("Reflect","ownKeys")||function(t){var e=o.f(c(t)),n=i.f;return n?e.concat(n(t)):e}},function(t,e,n){var r=n(14),o=Math.max,i=Math.min;t.exports=function(t,e){var n=r(t);return n<0?o(n+e,0):i(n,e)}},function(t,e,n){var r=n(1),o=/#|\.prototype\./,i=function(t,e){var n=u[c(t)];return n==f||n!=a&&("function"==typeof e?r(e):!!e)},c=i.normalize=function(t){return String(t).replace(o,".").toLowerCase()},u=i.data={},a=i.NATIVE="N",f=i.POLYFILL="P";t.exports=i},function(t,e,n){var r=n(7),o=n(49),i=n(3)("species");t.exports=function(t,e){var n;return o(t)&&("function"!=typeof(n=t.constructor)||n!==Array&&!o(n.prototype)?r(n)&&null===(n=n[i])&&(n=void 0):n=void 0),new(void 0===n?Array:n)(0===e?0:e)}},function(t,e,n){var r=n(6),o=n(8),i=n(4),c=n(51);t.exports=r?Object.defineProperties:function(t,e){i(t);for(var n,r=c(e),u=r.length,a=0;u>a;)o.f(t,n=r[a++],e[n]);return t}},function(t,e,n){var r=n(29);t.exports=r("document","documentElement")},function(t,e,n){"use strict";var r=n(4);t.exports=function(){var t=r(this),e="";return t.global&&(e+="g"),t.ignoreCase&&(e+="i"),t.multiline&&(e+="m"),t.dotAll&&(e+="s"),t.unicode&&(e+="u"),t.sticky&&(e+="y"),e}},function(t,e){t.exports={CSSRuleList:0,CSSStyleDeclaration:0,CSSValueList:0,ClientRectList:0,DOMRectList:0,DOMStringList:0,DOMTokenList:1,DataTransferItemList:0,FileList:0,HTMLAllCollection:0,HTMLCollection:0,HTMLFormElement:0,HTMLSelectElement:0,MediaList:0,MimeTypeArray:0,NamedNodeMap:0,NodeList:1,PaintRequestList:0,Plugin:0,PluginArray:0,SVGLengthList:0,SVGNumberList:0,SVGPathSegList:0,SVGPointList:0,SVGStringList:0,SVGTransformList:0,SourceBufferList:0,StyleSheetList:0,TextTrackCueList:0,TextTrackList:0,TouchList:0}},,function(t,e,n){var r=n(3),o=n(39),i=n(8),c=r("unscopables"),u=Array.prototype;null==u[c]&&i.f(u,c,{configurable:!0,value:o(null)}),t.exports=function(t){u[c][t]=!0}},,function(t,e,n){"use strict";var r=n(47).forEach,o=n(55),i=n(41),c=o("forEach"),u=i("forEach");t.exports=c&&u?[].forEach:function(t){return r(this,t,arguments.length>1?arguments[1]:void 0)}},function(t,e,n){"use strict";var r=n(1);function o(t,e){return RegExp(t,e)}e.UNSUPPORTED_Y=r((function(){var t=o("a","y");return t.lastIndex=2,null!=t.exec("abcd")})),e.BROKEN_CARET=r((function(){var t=o("^r","gy");return t.lastIndex=2,null!=t.exec("str")}))},function(t,e,n){"use strict";n(56);var r=n(13),o=n(1),i=n(3),c=n(31),u=n(5),a=i("species"),f=!o((function(){var t=/./;return t.exec=function(){var t=[];return t.groups={a:"7"},t},"7"!=="".replace(t,"$<a>")})),s="$0"==="a".replace(/./,"$0"),l=!o((function(){var t=/(?:)/,e=t.exec;t.exec=function(){return e.apply(this,arguments)};var n="ab".split(t);return 2!==n.length||"a"!==n[0]||"b"!==n[1]}));t.exports=function(t,e,n,p){var d=i(t),v=!o((function(){var e={};return e[d]=function(){return 7},7!=""[t](e)})),h=v&&!o((function(){var e=!1,n=/a/;return"split"===t&&((n={}).constructor={},n.constructor[a]=function(){return n},n.flags="",n[d]=/./[d]),n.exec=function(){return e=!0,null},n[d](""),!e}));if(!v||!h||"replace"===t&&(!f||!s)||"split"===t&&!l){var m=/./[d],y=n(d,""[t],(function(t,e,n,r,o){return e.exec===c?v&&!o?{done:!0,value:m.call(e,n,r)}:{done:!0,value:t.call(n,e,r)}:{done:!1}}),{REPLACE_KEEPS_$0:s}),g=y[0],x=y[1];r(String.prototype,t,g),r(RegExp.prototype,d,2==e?function(t,e){return x.call(t,this,e)}:function(t){return x.call(t,this)})}p&&u(RegExp.prototype[d],"sham",!0)}},function(t,e,n){"use strict";var r=n(52).charAt;t.exports=function(t,e,n){return e+(n?r(t,e).length:1)}},function(t,e,n){var r=n(12),o=n(31);t.exports=function(t,e){var n=t.exec;if("function"==typeof n){var i=n.call(t,e);if("object"!=typeof i)throw TypeError("RegExp exec method returned something other than an Object or null");return i}if("RegExp"!==r(t))throw TypeError("RegExp#exec called on incompatible receiver");return o.call(t,e)}},,,,,,,,,function(t,e,n){"use strict";var r=n(9),o=n(72);r({target:"Array",proto:!0,forced:[].forEach!=o},{forEach:o})},function(t,e,n){"use strict";var r=n(9),o=n(40).indexOf,i=n(55),c=n(41),u=[].indexOf,a=!!u&&1/[1].indexOf(1,-0)<0,f=i("indexOf"),s=c("indexOf",{ACCESSORS:!0,1:0});r({target:"Array",proto:!0,forced:a||!f||!s},{indexOf:function(t){return a?u.apply(this,arguments)||0:o(this,t,arguments.length>1?arguments[1]:void 0)}})},function(t,e){t.exports="\t\n\v\f\r                　\u2028\u2029\ufeff"},function(t,e,n){"use strict";var r=n(74),o=n(4),i=n(19),c=n(18),u=n(14),a=n(11),f=n(75),s=n(76),l=Math.max,p=Math.min,d=Math.floor,v=/\$([$&'`]|\d\d?|<[^>]*>)/g,h=/\$([$&'`]|\d\d?)/g;r("replace",2,(function(t,e,n,r){return[function(n,r){var o=a(this),i=null==n?void 0:n[t];return void 0!==i?i.call(n,o,r):e.call(String(o),n,r)},function(t,i){if(r.REPLACE_KEEPS_$0||"string"==typeof i&&-1===i.indexOf("$0")){var a=n(e,t,this,i);if(a.done)return a.value}var d=o(t),v=String(this),h="function"==typeof i;h||(i=String(i));var y=d.global;if(y){var g=d.unicode;d.lastIndex=0}for(var x=[];;){var b=s(d,v);if(null===b)break;if(x.push(b),!y)break;""===String(b[0])&&(d.lastIndex=f(v,c(d.lastIndex),g))}for(var S,w="",E=0,O=0;O<x.length;O++){b=x[O];for(var j=String(b[0]),L=l(p(u(b.index),v.length),0),A=[],T=1;T<b.length;T++)A.push(void 0===(S=b[T])?S:String(S));var _=b.groups;if(h){var P=[j].concat(A,L,v);void 0!==_&&P.push(_);var k=String(i.apply(void 0,P))}else k=m(j,v,L,A,_,i);L>=E&&(w+=v.slice(E,L)+k,E=L+j.length)}return w+v.slice(E)}];function m(t,n,r,o,c,u){var a=r+t.length,f=o.length,s=h;return void 0!==c&&(c=i(c),s=v),e.call(u,s,(function(e,i){var u;switch(i.charAt(0)){case"$":return"$";case"&":return t;case"`":return n.slice(0,r);case"'":return n.slice(a);case"<":u=c[i.slice(1,-1)];break;default:var s=+i;if(0===s)return e;if(s>f){var l=d(s/10);return 0===l?e:l<=f?void 0===o[l-1]?i.charAt(1):o[l-1]+i.charAt(1):e}u=o[s-1]}return void 0===u?"":u}))}}))},function(t,e,n){var r=n(0),o=n(68),i=n(72),c=n(5);for(var u in o){var a=r[u],f=a&&a.prototype;if(f&&f.forEach!==i)try{c(f,"forEach",i)}catch(t){f.forEach=i}}},,,,,,,,,,,,,,,function(t,e,n){var r=n(11),o="["+n(87)+"]",i=RegExp("^"+o+o+"*"),c=RegExp(o+o+"*$"),u=function(t){return function(e){var n=String(r(e));return 1&t&&(n=n.replace(i,"")),2&t&&(n=n.replace(c,"")),n}};t.exports={start:u(1),end:u(2),trim:u(3)}},function(t,e,n){n(106),n(112),t.exports=n(113)},function(t,e,n){n(107),n(85),n(86),n(108),n(56),n(88),n(110),n(89),function(t,e,n){"use strict";function r(t){var e=null;"prev"==t?e=document.querySelector("#page_prev"):"next"==t&&(e=document.querySelector("#page_next")),e&&(e.href?window.location.href=e.href:console.warn("Attempt to redirect failed; Page not set."))}t(".file > a").each((function(){t(this).attr("download","")})),t('[data-toggle="collapse"]').on("click",(function(){window.dispatchEvent(new Event("resize"))})),document.addEventListener("keydown",(function(t){if(!function(){var t=document.activeElement;if(t&&-1!==["input","select","button","textarea"].indexOf(t.tagName.toLowerCase()))return!0;return!1}()&&t.isTrusted){var e=t.which,n=t.location,o=(t.key,t.ctrlKey||t.metaKey),i=t.shiftKey;if(37==e&&o||4==e&&3==n)r("prev"),t.preventDefault();else if(39==e&&o||6==e&&3==n)r("next"),t.preventDefault();else if(70==e&&o&&i)show_message('<form action="/onlinecourses/search/node" method="get" id="search-modal-form" accept-charset="UTF-8" data-drupal-selector="search-modal-form" class="search-form search-block-form" data-drupal-form-fields="edit-modal-keys"><fieldset class="js-form-item js-form-type-search form-type-search js-form-item-keys form-item-keys form-no-label form-group"><label for="edit-modal-keys" class="sr-only">Search</label><input title="Enter the terms you wish to search for." data-drupal-selector="edit-modal-keys" class="form-autocomplete form-search form-control ui-autocomplete-input" data-key="search_block" type="search" id="edit-modal-keys" name="keys" value="" size="15" maxlength="128" autocomplete="on"></fieldset><div data-drupal-selector="edit-actions" class="form-actions js-form-wrapper form-goup" id="edit-modal-actions"><button data-drupal-selector="edit-submit" type="submit" id="edit-modal-submit" class="button js-form-submit form-submit btn btn-primary">Search</button></div></form>',"Search"),jQuery("#edit-modal-keys").focus();else if(112==e||191==e&&i)jQuery("#keyboardShortcuts").modal("toggle");else if(27!=e&&67!=e&&88!=e||i||o){if(80==e&&o){var c=jQuery(".book-printer").find("a")[0].href;c&&(window.open(c),t.preventDefault())}}else jQuery("#message_modal").modal("hide")}})),t(".logout-btn").on("click",(function(){jQuery.post(n.path.baseUrl+"/user/logout",{target:"drupal"}).then((function(){setTimeout(location.reload(),5e3)}))})),window.show_message=function(t,e,n){jQuery("#message_modal .modal-title").html(e),jQuery("#message_modal .modal-body").html(t),jQuery("#message_modal").modal("show"),MathJax.Hub.Queue(["Typeset",MathJax.Hub,"message_modal"]),jQuery("#message_modal").attr("aria-hidden","false")}}(jQuery,Drupal,drupalSettings),document.addEventListener("DOMContentLoaded",(function(){document.querySelectorAll(".accordion-header button").forEach((function(t){t.addEventListener("click",(function(){document.getElementById(this.getAttribute("data-target").replace("#","")).classList.contains("show")?t.querySelector(".accordion-toggle-icon svg").setAttribute("data-fa-transform","rotate-0"):t.querySelector(".accordion-toggle-icon svg").setAttribute("data-fa-transform","rotate-90")}))}))}),!1)},function(t,e,n){"use strict";var r=n(9),o=n(47).find,i=n(70),c=n(41),u=!0,a=c("find");"find"in[]&&Array(1).find((function(){u=!1})),r({target:"Array",proto:!0,forced:u||!a},{find:function(t){return o(this,t,arguments.length>1?arguments[1]:void 0)}}),i("find")},function(t,e,n){var r=n(9),o=n(109);r({global:!0,forced:parseFloat!=o},{parseFloat:o})},function(t,e,n){var r=n(0),o=n(104).trim,i=n(87),c=r.parseFloat,u=1/c(i+"-0")!=-1/0;t.exports=u?function(t){var e=o(String(t)),n=c(e);return 0===n&&"-"==e.charAt(0)?-0:n}:c},function(t,e,n){"use strict";var r=n(9),o=n(104).trim;r({target:"String",proto:!0,forced:n(111)("trim")},{trim:function(){return o(this)}})},function(t,e,n){var r=n(1),o=n(87);t.exports=function(t){return r((function(){return!!o[t]()||"​᠎"!="​᠎"[t]()||o[t].name!==t}))}},function(t,e){!function(){var t=document.getElementsByClassName("scroll-to-top")[0],e=document.getElementsByClassName("site-footer")[0],n=!1;function r(t){var e;t.preventDefault(),e="#page-wrapper",document.querySelector(e).scrollIntoView({behavior:"smooth",block:"start",inline:"start"})}function o(){var r=document.documentElement.offsetHeight,o=window.scrollY||document.documentElement.scrollTop,i=window.innerHeight,c=window.scrollY||window.pageYOffset||document.body.scrollTop+(document.documentElement&&document.documentElement.scrollTop||0),u=e.offsetTop||r;o>300?t.classList.add("show"):t.classList.remove("show"),t.style.bottom=u<=i+c?c-u+i+24+"px":"1.5rem",n=!1}t&&(window.addEventListener("scroll",(function(){n||(n=!0,window.requestAnimationFrame?window.requestAnimationFrame(o):setTimeout(o,250))})),t.addEventListener("click",r),t.addEventListener("touchstart",r))}()},function(t,e){!function(){for(var t=document.querySelectorAll("td"),e=0;e<t.length;e++)t[e].addEventListener("mouseover",r),t[e].addEventListener("mouseleave",o);function n(t,e){for(var n=t.parentNode;n.tagName.toLowerCase()!=e.toLowerCase();)n=n.parentNode;return n}function r(){var t=n(this,"table").querySelectorAll("colgroup")[this.cellIndex];t&&t.classList.add("hover"),this.classList.add("hover")}function o(){var t=n(this,"table").querySelectorAll("colgroup")[this.cellIndex];t&&t.classList.remove("hover"),this.classList.remove("hover")}}()}]);
/* Source and licensing information for the above line(s) can be found at https://online.stat.psu.edu/stat510/themes/onlinecourses_theme/dist/js/main.bundle.js. */;
