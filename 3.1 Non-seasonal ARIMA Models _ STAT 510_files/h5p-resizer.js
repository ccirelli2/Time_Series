// H5P iframe Resizer
'use strict';(function(){if(window.postMessage&&window.addEventListener&&!window.h5pResizerInitialized){window.h5pResizerInitialized=!0;var a={hello:function(f,g,h){f.style.width='100%';var j=function(){f.contentWindow?h('resize'):window.removeEventListener('resize',j)};window.addEventListener('resize',j,!1),h('hello')},prepareResize:function(f,g,h){(f.clientHeight!==g.scrollHeight||g.scrollHeight!==g.clientHeight)&&(f.style.height=g.clientHeight+'px',h('resizePrepared'))},resize:function(f,g){f.style.height=g.scrollHeight+'px'}};window.addEventListener('message',function(g){if('h5p'===g.data.context){for(var h,j=document.getElementsByTagName('iframe'),k=0;k<j.length;k++)if(j[k].contentWindow===g.source){h=j[k];break}!h||a[g.data.action]&&a[g.data.action](h,g.data,function(m,n){void 0===n&&(n={}),n.action=m,n.context='h5p',g.source.postMessage(n,g.origin)})}},!1);for(var c=document.getElementsByTagName('iframe'),d={context:'h5p',action:'ready'},e=0;e<c.length;e++)-1!==c[e].src.indexOf('h5p')&&c[e].contentWindow.postMessage(d,'*')}})();