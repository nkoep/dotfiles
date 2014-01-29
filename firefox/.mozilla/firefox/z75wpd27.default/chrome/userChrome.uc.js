/* compact menu */
(function()
{
  var compact = document.createElement("menu");
  compact.setAttribute("label", "");

  var mPopup = document.createElement("menupopup");

  var menubar = document.getElementById("main-menubar");
  var menus = menubar.childNodes.length;
  
  for (var i = 0; i < menus; ++i)
  {
    mPopup.appendChild(menubar.firstChild);
  }

  compact.appendChild(mPopup);
  menubar.appendChild(compact);

})();

/* replace url on start page */
function replace_url()
{
	var obj = document.getElementById("urlbar");
	if( gBrowser.selectedTab.label == "_bla" )
	{
		obj.value = "";
	}
}
gBrowser.addEventListener("TabSelect", replace_url, true);
gBrowser.addEventListener("load", replace_url, true);

/* open urls from start page in new tab */
function startpage_newtab(event)
{
	if( event.which == 13 )
	{
		if( gBrowser.selectedTab.label == "_bla" )
		{
			var obj = document.getElementById( "urlbar" );
			var tab = gBrowser.addTab( obj.value );
			gBrowser.selectedTab = tab;
		}
	}
}
window.addEventListener( "keypress", startpage_newtab, true );
window.addEventListener( "click", startpage_newtab, true );

/* replace tab string on empty tabs */
function replace_tabtext ()
{
	if( gBrowser.selectedTab.label == "New Tab" )
	{
		gBrowser.selectedTab.label = "_brap";
	}
}
gBrowser.addEventListener("TabSelect", replace_tabtext, true);
gBrowser.addEventListener("load", replace_tabtext, true);

/* open bookmarks in new tabs */
eval("PlacesUIUtils.openNodeIn = " + PlacesUIUtils.openNodeIn.toString().replace("openUILinkIn(aNode.uri, aWhere);","if ((gBrowser.currentURI.spec != 'about:blank' || gBrowser.webProgress.isLoadingDocument) && (aNode.uri.indexOf('javascript:') == -1)) {openUILinkIn(aNode.uri, 'tab');}else{openUILinkIn(aNode.uri, aWhere);}"));
