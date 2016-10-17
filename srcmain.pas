{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Url at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Dariusz Rorat
  Revision: Ircama

}

unit srcmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, StdCtrls, ExtDlgs, Buttons, EditBtn, DateUtils, LCLType,
  ComCtrls, ActnList, Spin, ZVDateTimePicker, ubarcodes, LazUTF8, RTTICtrls;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnQuit: TBitBtn;
    btnLoadText: TBitBtn;
    cboCodeType: TComboBox;
    cboECCLevel: TComboBox;
    cboBarFormat: TComboBox;
    cboUrlType: TComboBox;
    mailOpt: TCheckBox;
    smsOpt: TCheckBox;
    geoLocation: TEdit;
    geoQuery: TEdit;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    networkSsid: TEdit;
    networkPassword: TEdit;
    networkHidden: TCheckBox;
    networkProt: TComboBox;
    eventLocalTimeOffset: TEdit;
    eventStartAllDay: TCheckBox;
    contactName: TEdit;
    contactHomeMobile: TEdit;
    contactSurname: TEdit;
    contactEmail: TEdit;
    contactCompany: TEdit;
    contactTitle: TEdit;
    contactWorkPhone: TEdit;
    contactWorkMobile: TEdit;
    contactWorkFax: TEdit;
    contactHomePhone: TEdit;
    eventDescr: TEdit;
    eventDescription: TTabSheet;
    eventLocation: TEdit;
    eventSummary: TEdit;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Memo: TTabSheet;
    contactWorkAddress: TMemo;
    contactHomeAddress: TMemo;
    contactNote: TMemo;
    contactUrl: TMemo;
    TabSheet2: TTabSheet;
    textual: TMemo;
    mmsCkb: TCheckBox;
    crNlCkb: TCheckBox;
    emailTo: TEdit;
    emailCc: TEdit;
    geoAltitude: TEdit;
    geoLongitude: TEdit;
    geoLatitude: TEdit;
    Geo: TTabSheet;
    InMemo: TMemo;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    TabSheet1: TTabSheet;
    Tel: TTabSheet;
    urlContent: TMemo;
    Label7: TLabel;
    smsBody: TMemo;
    emailBody: TMemo;
    emailSubject: TMemo;
    smsTo: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog: TOpenDialog;
    formatOptions: TPageControl;
    SavePictureDialog: TSavePictureDialog;
    Shape2: TShape;
    email: TTabSheet;
    SMS: TTabSheet;
    Url: TTabSheet;
    telNum: TMemo;
    eventStartDate: TZVDateTimePicker;
    eventEndDate: TZVDateTimePicker;

    procedure btnLoadTextClick(Sender: TObject);
    procedure btnQuitClick(Sender: TObject);
    procedure cboCodeTypeChange(Sender: TObject);
    procedure cboCodeTypeEditingDone(Sender: TObject);
    procedure cboECCLevelChange(Sender: TObject);
    procedure cboECCLevelEditingDone(Sender: TObject);
    procedure cboBarFormatChange(Sender: TObject);
    procedure cboUrlTypeChange(Sender: TObject);
    procedure geoLocationChange(Sender: TObject);
    procedure geoQueryChange(Sender: TObject);
    procedure mailOptChange(Sender: TObject);
    procedure networkProtChange(Sender: TObject);
    procedure contactCompanyChange(Sender: TObject);
    procedure contactEmailChange(Sender: TObject);
    procedure contactHomeAddressChange(Sender: TObject);
    procedure contactHomeMobileChange(Sender: TObject);
    procedure contactHomePhoneChange(Sender: TObject);
    procedure contactNameChange(Sender: TObject);
    procedure contactNoteChange(Sender: TObject);
    procedure contactUrlChange(Sender: TObject);
    procedure contactWorkFaxChange(Sender: TObject);
    procedure contactWorkMobileChange(Sender: TObject);
    procedure contactWorkPhoneChange(Sender: TObject);
    procedure contactSurnameChange(Sender: TObject);
    procedure contactTitleChange(Sender: TObject);
    procedure contactWorkAddressChange(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure eventDescrChange(Sender: TObject);
    procedure eventEndAllDayChange(Sender: TObject);
    procedure eventEndDateChange(Sender: TObject);
    procedure eventEndHourChange(Sender: TObject);
    procedure eventEndMinuteChange(Sender: TObject);
    procedure eventLocalTimeOffsetChange(Sender: TObject);
    procedure eventLocationChange(Sender: TObject);
    procedure eventStartAllDayChange(Sender: TObject);
    procedure eventStartDateChange(Sender: TObject);
    procedure eventStartMinuteChange(Sender: TObject);
    procedure eventSummaryChange(Sender: TObject);
    procedure mmsCkbChange(Sender: TObject);
    procedure crNlCkbChange(Sender: TObject);
    procedure CalStartDateChange(Sender: TObject);
    procedure emailToChange(Sender: TObject);
    procedure geoAltitudeChange(Sender: TObject);
    procedure geoLatitudeChange(Sender: TObject);
    procedure emailBodyChange(Sender: TObject);
    procedure emailSubjectChange(Sender: TObject);
    procedure emailCcChange(Sender: TObject);
    procedure formatOptionsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure geoLongitudeChange(Sender: TObject);
    procedure InMemoChange(Sender: TObject);
    procedure msgBoxChange(Sender: TObject);
  const
    sLineBreak = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF}
                 {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};
  procedure networkHiddenChange(Sender: TObject);
  procedure networkPasswordChange(Sender: TObject);
  procedure networkSsidChange(Sender: TObject);
  procedure smsOptChange(Sender: TObject);
  private
    QR: TBarcodeQR;
    Aztec: TBarcodeAztec;
    DataMatrix: TBarCodeDataMatrix;
    CurrentGenerator: TLazBarcodeCustomText;

    procedure SetupGenerator;
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses
  programconsts, gettext, translations;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.SetupGenerator;
var
  intIndex: Integer;
  intEccLevel: Integer;
  i: Integer;
  strMsg: UTF8String;
  strMsgF: UTF8String;
  fromdate: UTF8String;
  todate: UTF8String;
  networkType: UTF8String;
  charset: UTF8String;
  messageOptTo: UTF8String;
begin
  try
    intIndex    := cboBarFormat.ItemIndex;
    intEccLevel := cboEccLevel.ItemIndex;
    if intEccLevel < 0 then
      intEccLevel := 0;
    if cboCodeType.ItemIndex = 1 then
       charset := ';ENCODING=QUOTED-PRINTABLE;CHARSET=UTF-8'
    else
       charset := '';

        case formatOptions.TabIndex of
             0: begin  // default
                strMsg := InMemo.Text;
                end;
             1: begin  // sms or mms
                if smsOpt.Checked then messageOptTo := 'TO' else messageOptTo := '';
                if mmsCkb.Checked
                   then
                       strMsg := 'MMS' + messageOptTo + ':' + smsTo.Text + ':' + smsBody.Text
                   else
                       strMsg := 'SMS' + messageOptTo + ':' + smsTo.Text + ':' + smsBody.Text;
                end;
             2: begin  // email
                if mailOpt.Checked then messageOptTo := 'to' else messageOptTo := '';
                strMsg := 'mail' + messageOptTo + ':' + emailTo.Text + '?cc=' + emailCc.Text + '&subject=' + emailSubject.Text + '&body=' + emailBody.Text;
                end;
             3: begin  // Url
                strMsg := urlContent.Text;
                end;
             4: begin  // tel
                strMsg := 'TEL:' + telNum.Text;
                end;
             5: begin  // geo
                if geoAltitude.Text <> ''
                   then strMsg := 'geo:' + geoLatitude.Text + ',' + geoLongitude.Text + ',' + geoAltitude.Text
                   else strMsg := 'geo:' + geoLatitude.Text + ',' + geoLongitude.Text;
                if geoQuery.Text <> '' then strMsg := strMsg + '?q=' + geoQuery.Text;
                if geoLocation.Text <> '' then
                    begin
                    strMsg := geoLocation.Text;
                    strMsg := StringReplace(strMsg, ' ', '+',[rfReplaceAll]);
                    strMsg := StringReplace(strMsg, ',', '+',[rfReplaceAll]);
                    strMsg := StringReplace(strMsg, ';', '+',[rfReplaceAll]);
                    strMsg := StringReplace(strMsg, '.', '+',[rfReplaceAll]);
                    strMsg := StringReplace(strMsg, '?', '+',[rfReplaceAll]);

                    strMsg:= StringReplace(strMsg,'%','%25',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'<','%3C',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'>','%3E',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'~','%7E',[rfReplaceAll]);
                    // strMsg:= StringReplace(strMsg,'.','%2E',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'"','%22',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'{','%7B',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'}','%7D',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'|','%7C',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'\','%5C',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'-','%2D',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'`','%60',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'_','%5F',[rfReplaceAll]);
                    strMsg:= StringReplace(strMsg,'^','%5E',[rfReplaceAll]);

                    for i := 1 to 100 do
                        strMsg := StringReplace(strMsg, '++', '+',[rfReplaceAll]);
                    strMsg := 'https://maps.google.com/?q=' + strMsg;
                    end
                end;
             6: begin  // VCARD contact
                strMsg := 'BEGIN:VCARD' + sLineBreak + 'VERSION:3.0:' + sLineBreak +
                'N' + charset + ':' + Trim(contactSurname.Text + ';' + contactName.Text) +
                sLineBreak + 'FN' + charset + ':' + Trim(contactName.Text + ' ' + contactSurname.Text) +
                sLineBreak + 'ORG' + charset + ':' + contactCompany.Text +
                sLineBreak + 'TITLE' + charset + ':' + contactTitle.Text +
                sLineBreak + 'MEMO' + charset + ':' + StringReplace(contactNote.Text, sLineBreak, ';',[rfReplaceAll]) +
                sLineBreak + 'URL' + charset + ':' + StringReplace(contactUrl.Text, sLineBreak, ';',[rfReplaceAll]) +
                sLineBreak + 'TEL;TYPE=WORK,VOICE:' + contactWorkPhone.Text +
                sLineBreak + 'TEL;TYPE=HOME,VOICE:' + contactHomePhone.Text +
                sLineBreak + 'TEL;TYPE=WORK,CELL:' + contactWorkMobile.Text +
                sLineBreak + 'TEL;TYPE=HOME,CELL:' + contactHomeMobile.Text +
                sLineBreak + 'TEL;TYPE=FAX:' + contactWorkFax.Text +
                sLineBreak + 'ADR;TYPE=WORK' + charset + ':' + StringReplace(contactWorkAddress.Text, sLineBreak, ';',[rfReplaceAll]) +
                sLineBreak + 'ADR;TYPE=HOME' + charset + ':' + StringReplace(contactHomeAddress.Text, sLineBreak, ';',[rfReplaceAll]) +
                sLineBreak + 'EMAIL;TYPE=PREF,INTERNET' + charset + ':' + contactEmail.Text +
                sLineBreak + 'END:VCARD';
                end;
             7: begin  // event (VEVENT without VCALENDAR envelope)
                eventLocalTimeOffset.Text := StringReplace('UTC+' + IntToStr(GetLocalTimeOffset div 60 * -1), '+-', '-',[rfReplaceAll]);
                fromdate := '';
                todate := '';
                if eventStartDate.DateTime = NullDate then eventStartDate.DateTime := Now;
                if ((eventEndDate.DateTime = NullDate) and (eventStartDate.DateTime <> NullDate)) or (eventStartDate.DateTime>eventEndDate.DateTime) then eventEndDate.DateTime:=eventStartDate.DateTime;
                if eventStartDate.DateTime <> NullDate then
                    if eventStartAllDay.Checked
                       then
                           begin
                           fromdate := sLineBreak + 'DTSTART' + charset + ':' + FormatDateTime('YYYYMMDD', LocalTimeToUniversal(eventStartDate.DateTime));
                           eventStartDate.Kind := dtkDate;
                           end
                       else
                           begin
                           fromdate := sLineBreak + 'DTSTART' + charset + ':' + FormatDateTime('YYYYMMDD"T"HHmmss"Z"', LocalTimeToUniversal(eventStartDate.DateTime));
                           eventStartDate.Kind := dtkDateTime;
                           end;
                if eventEndDate.DateTime <> NullDate then
                    if eventStartAllDay.Checked
                   then
                       begin
                       todate := sLineBreak + 'DTEND' + charset + ':' + FormatDateTime('YYYYMMDD', LocalTimeToUniversal(eventEndDate.DateTime));
                       eventEndDate.Kind := dtkDate;
                       end
                   else
                       begin
                       todate := sLineBreak + 'DTEND' + charset + ':' + FormatDateTime('YYYYMMDD"T"HHmmss"Z"', LocalTimeToUniversal(eventEndDate.DateTime));
                       eventEndDate.Kind := dtkDateTime;
                       end;
                strMsg := 'BEGIN:VEVENT' +
                sLineBreak + 'SUMMARY' + charset + ':' + eventSummary.Text +
                fromdate +
                todate +
                sLineBreak + 'LOCATION' + charset + ':' + eventLocation.Text +
                sLineBreak + 'DESCRIPTION' + charset + ':' + eventDescr.Text +
                sLineBreak + 'END:VEVENT';
                end;
             8: begin  // wifi
                case networkProt.ItemIndex of
                     0: networkType := ';T:' + 'WEP';
                     1: networkType := ';T:' + 'WPA';
                     2: networkType := '';
                end;
                strMsg := 'WIFI:S:' + networkSsid.Text + networkType;
                if networkPassword.text <> '' then strMsg := strMsg + ';P:' + networkPassword.Text;
                if networkHidden.Checked then strMsg := strMsg + ';H:true';
                strMsg := strMsg + ';;';
                end;
        end;

        case cboCodeType.ItemIndex of
             0: strMsgF := strMsg;
             1: strMsgF := strMsg;
             2: strMsgF := UTF8ToAnsi(strMsg);
             //3: strMsgF := UTF16ToUTF8(strMsg);
        end;

        if not (crNlCkb.Checked) then
           strMsgF  := StringReplace(strMsgF, sLineBreak, #10,[rfReplaceAll]);

        case intIndex of
            0:
            begin
              CurrentGenerator := QR;
              QR.Visible := true;
              Aztec.Visible := false;
              Datamatrix.Visible := false;
              Label2.Visible := true;
              cboEcclevel.Visible := true;
              QR.ECCLevel := TBarcodeQR_ECCLevel(intEccLevel);
              QR.Text := strMsgF;
              textual.Visible := false;
            end;
            1:
            begin
              CurrentGenerator := Aztec;
              QR.Visible := false;
              Aztec.Visible := true;
              Datamatrix.Visible := false;
              Label2.Visible := false;
              cboEcclevel.Visible := false;
              Aztec.Text := strMsgF;
              textual.Visible := false;
            end;
            2:
            begin
              CurrentGenerator := DataMatrix;
              QR.Visible := false;
              Aztec.Visible := false;
              Datamatrix.Visible := true;
              Label2.Visible := false;
              cboEcclevel.Visible := false;
              Datamatrix.Text := strMsgF;
              textual.Visible := false;
            end;
            else
            begin
              QR.Visible := false;
              Aztec.Visible := false;
              Label2.Visible := false;
              cboEcclevel.Visible := false;
              Datamatrix.Visible := false;
              textual.Visible := True;
              textual.Text:=strMsg;
            end;
          end;

  except
    MessageDlg(strConvertError, mtError, [mbOK], 0);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  PODirectory, Lang, FallbackLang: string;
begin
  KeyPreview := True;
  PODirectory := '.\languages\';
  GetLanguageIDs(Lang, FallbackLang); // in unit gettext
  TranslateUnitResourceStrings('LCLStrConsts',
    PODirectory + 'lclstrconsts.%s.po', Lang, FallbackLang);
  TranslateUnitResourceStrings('ProgramConsts',
    PODirectory + 'messages.%s.po', Lang, FallbackLang);

  Caption := strCaption;
  Label1.Caption := strCodeType;
  Label2.Caption := strEccLevel;
  btnLoadText.Caption := strLoadFromText;
  btnQuit.Caption := strQuit;
  OpenDialog.Filter := strFilter;
  OpenDialog.Title := strOpenDialogTitle;

  QR := TBarcodeQR.Create(Self);
  QR.Parent := frmMain;
  QR.Left := Shape2.Left + 8;
  QR.Top := Shape2.Top + 8;
  QR.Width := Shape2.Width - 16;
  QR.Height := Shape2.Height - 16;
  QR.Visible := true;
  QR.Text := '';

  Aztec := TBarcodeAztec.Create(Self);
  Aztec.Parent := frmMain;
  Aztec.Left := Shape2.Left + 8;
  Aztec.Top := Shape2.Top + 8;
  Aztec.Width := Shape2.Width - 16;
  Aztec.Height := Shape2.Height - 16;
  Aztec.Visible := false;
  Aztec.Text := '';

  DataMatrix := TBarcodeDataMatrix.Create(Self);
  DataMatrix.Parent := frmMain;
  DataMatrix.Left := Shape2.Left + 8;
  DataMatrix.Top := Shape2.Top + 8;
  DataMatrix.Width := Shape2.Width - 16;
  DataMatrix.Height := Shape2.Height - 16;
  DataMatrix.Visible := false;
  DataMatrix.Text := '';
  frmMain.BringToFront;
  frmMain.ShowOnTop;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (frmMain.ActiveControl = formatOptions) and (key = VK_DOWN) and (formatOptions.TabIndex = 0) then InMemo.SetFocus;
  if (ssAlt in Shift) and (lowercase(Chr(key)) = 'e') then cboECCLevel.SetFocus;
  if (ssAlt in Shift) and (lowercase(Chr(key)) = 'f') then cboCodeType.SetFocus;
  if (ssAlt in Shift) and (lowercase(Chr(key)) = 't') then InMemo.SetFocus;
  if (ssAlt in Shift) and (lowercase(Chr(key)) = 'm') then formatOptions.SetFocus;
  if (ssAlt in Shift) and (lowercase(Chr(key)) = 'q') then cboBarFormat.SetFocus;
  if (ssAlt in Shift) and (lowercase(Chr(key)) = 'l') then btnLoadText.SetFocus;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    if MessageDlg('Question', 'Do you wish to quit?', mtConfirmation,
     [mbYes, mbNo],0) = mrYes
    then Close;
  end;
end;

procedure TfrmMain.geoLongitudeChange(Sender: TObject);
begin
  geoLocation.Text := '';
  SetupGenerator;
end;

procedure TfrmMain.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.cboCodeTypeChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.cboBarFormatChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.cboUrlTypeChange(Sender: TObject);
begin
  case cboUrlType.ItemIndex of
      0: urlContent.Text := '';
      1: urlContent.Text := 'http://';
      2: urlContent.Text := 'https://';
      3: urlContent.Text := 'music:';
      4: urlContent.Text := 'videos:';
      5: urlContent.Text := 'youtube://';
      6: urlContent.Text := 'ibooks://';
      7: urlContent.Text := 'fb://';
      8: urlContent.Text := 'mailto:';
      9: urlContent.Text := 'ftp://';
      10: urlContent.Text := 'TEL:';
      11: urlContent.Text := 'SMSTO:';
      12: urlContent.Text := 'MMSTO:';
      13: urlContent.Text := 'facetime://';
      14: urlContent.Text := 'Skype:';
      15: urlContent.Text := 'itms://';
      16: urlContent.Text := 'market://';
      17: urlContent.Text := 'appworld://';
  end;
  SetupGenerator;
end;

procedure TfrmMain.geoLocationChange(Sender: TObject);
begin
  geoAltitude.Text := '';
  geoLatitude.Text := '';
  geoLongitude.Text := '';
  geoQuery.Text := '';

  SetupGenerator;
end;

procedure TfrmMain.geoQueryChange(Sender: TObject);
begin
  geoLocation.Text := '';
  SetupGenerator;
end;

procedure TfrmMain.mailOptChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.networkProtChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactCompanyChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactEmailChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactHomeAddressChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactHomeMobileChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactHomePhoneChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactNameChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactNoteChange(Sender: TObject);
begin
    SetupGenerator;
end;

procedure TfrmMain.contactUrlChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactWorkFaxChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactWorkMobileChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactWorkPhoneChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactSurnameChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactTitleChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.contactWorkAddressChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.Edit1Change(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventDescrChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventEndAllDayChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventEndDateChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventEndHourChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventStartMinuteChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventEndMinuteChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventLocalTimeOffsetChange(Sender: TObject);
begin

end;

procedure TfrmMain.eventLocationChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventStartAllDayChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventStartDateChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.eventSummaryChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.mmsCkbChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.cboCodeTypeEditingDone(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.cboECCLevelChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.cboECCLevelEditingDone(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.btnLoadTextClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    with TStringList.Create do
    begin
      LoadFromFile(UTF8ToSys(OpenDialog.FileName));
      InMemo.Text := SysToUTF8(Text);
      Free;
    end;
  end;
  SetupGenerator;
end;

procedure TfrmMain.crNlCkbChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.CalStartDateChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.emailToChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.geoAltitudeChange(Sender: TObject);
begin
  geoLocation.Text := '';
  SetupGenerator;
end;

procedure TfrmMain.emailBodyChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.emailSubjectChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.emailCcChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.geoLatitudeChange(Sender: TObject);
begin
  geoLocation.Text := '';
  SetupGenerator;
end;

procedure TfrmMain.formatOptionsChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.InMemoChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.msgBoxChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.networkSsidChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.smsOptChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.networkPasswordChange(Sender: TObject);
begin
  SetupGenerator;
end;

procedure TfrmMain.networkHiddenChange(Sender: TObject);
begin
  SetupGenerator;
end;

end.
