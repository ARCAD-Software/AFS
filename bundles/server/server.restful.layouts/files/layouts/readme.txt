Where to put XML Layouts pages definition, there is 3 options :


- Into this bundle (into the folder /files/layouts) :

If you want that this layout page must be accessible 
into any server where "dynamic editor" will be available. 
This should be the case for generic layouts like connection 
dialogs or reusable editor parts.



- Into a fragment linked to this bundle :

Fragment-Host: com.arcadsoftware.server.restful.layouts;bundle-version="[9.5.0,10.0.0)"
Place the layouts pages into a folder /files/layouts/

If theses pages are application specific or optional pages.



- Into a folder under the installation root folder.

See packaging facility of the current application and join a folder files/layouts/
to the installation root of your application.

(To be able to debug theses layouts file you will need to 
copy theses files into the Eclipse home directory, i.e. copy 
the whole "files/layouts" folder structure.)




You should combine one of the firsts option with the last one.


Modified layouts by end-users will always go into external folder.
The modified/external layouts are always privileged over internal
layouts files. thus if the internal layout file propose an update
it will not be used if the end-user has modified the previous 
version.
