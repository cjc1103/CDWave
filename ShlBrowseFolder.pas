unit ShlBrowseFolder;

interface

uses Windows, shlobj;

const BIF_NEWDIALOGSTYLE = $40;

function BrowseForFolder(hwndOwner: HWND;
        const browseTitle: String;
        const initialFolder: String =''): String;

implementation

///////////////////////////////////////////////////////////////////
// Call back function used to set the initial browse directory.
///////////////////////////////////////////////////////////////////
function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT;
        lParam, lpData: LPARAM): Integer stdcall;
begin
  if uMsg = BFFM_INITIALIZED then
    SendMessage(Wnd,BFFM_SETSELECTION,1,lpData);
  result := 0;
end;

///////////////////////////////////////////////////////////////////
// This function allows the user to browse for a folder
//
// Arguments:-
//    browseTitle : The title to display on the browse dialog.
//  initialFolder : Optional argument. Use to specify the folder
//                  initially selected when the dialog opens.
//
// Returns: The empty string if no folder was selected (i.e. if the
//          user clicked cancel), otherwise the full folder path.
///////////////////////////////////////////////////////////////////
function BrowseForFolder(hwndOwner: HWND;
        const browseTitle: String;
        const initialFolder: String =''): String;
var
  browse_info: TBrowseInfo;
  folder: array[0..MAX_PATH] of char;
  find_context: PItemIDList;
begin
  FillChar(browse_info,SizeOf(browse_info),#0);
  browse_info.pszDisplayName := @folder[0];
  browse_info.lpszTitle := PChar(browseTitle);
  browse_info.ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE;
  browse_info.hwndOwner := hwndOwner;
  if initialFolder <> '' then
  begin
    browse_info.lpfn := BrowseForFolderCallBack;
    browse_info.lParam := DWORD(PChar(initialFolder));
  end;
  find_context := SHBrowseForFolder(browse_info);
  if Assigned(find_context) then
  begin
    if SHGetPathFromIDList(find_context,folder) then
      result := folder
    else
      result := '';
    GlobalFreePtr(find_context);
  end
  else
    result := '';
end;

end.

