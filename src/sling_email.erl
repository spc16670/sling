%%%-------------------------------------------------------------------
%%% @author szymon.czaja
%%% @copyright (C) 2016, IONAS SOFTWARE LTD
%%% @doc
%%%
%%% @end
%%% Created : 08. Sep 2016 13:24
%%%-------------------------------------------------------------------
-module(sling_email).
-author("szymon.czaja").

%% API
-export([
	is_valid_email/1
	,is_valid_domain/1	
]).

%%
%% @doc
%%
is_valid_email(Addr) when is_binary(Addr) ->
	is_valid_email(binary_to_list(Addr));
is_valid_email(Addr) when is_list(Addr) ->
	{ok, LocalMP} = re:compile("^((?:(?:[^\"@\\.]+\\.?)|(?:\\.\"[^\"]+\"\\.))"
		"*(?:(?:\\.?\"[^\"]+\")|(?:[a-zA-Z0-9\\-_]+)))@[a-z0-9\\.\\-\\[\\]]+$"
 		, [caseless, anchored]),
	Match = re:run(Addr, LocalMP),
	is_valid_email(Match, Addr).

is_valid_email(nomatch, _) ->
	{false,""};
is_valid_email({match,Ranges}, Addr) when length(Ranges) =:= 2 ->
	{Start, End} = lists:nth(2, Ranges),
	is_valid_email(Start, End, Addr).

is_valid_email(0, End, Addr) when End > 0 ->
	domain_is_valid(string:sub_string(Addr, End + 2));
is_valid_email(_, _, _) ->
	{false,""}.

%%
%% @doc
%%
is_valid_domain(FQDN) ->
	DomRegex = "^((?:(?:[A-Z0-9][A-Z0-9\-]+\\.)+(?:"
		"AC|AD|AE|AERO|AF|AG|AI|AL|AM|AN|AO|AQ|AR|ARPA|AS|ASIA|AT|AU|A"
		"W|AX|AZ|BA|BB|BD|BE|BF|BG|BH|BI|BIZ|BJ|BM|BN|BO|BR|BS|BT|BV|B"
		"W|BY|BZ|CA|CAT|CC|CD|CF|CG|CH|CI|CK|CL|CM|CN|CO|COM|COOP|CR|C"
		"U|CV|CW|CX|CY|CZ|DE|DJ|DK|DM|DO|DZ|EC|EDU|EE|EG|ER|ES|ET|EU|F"
		"I|FJ|FK|FM|FO|FR|GA|GB|GD|GE|GF|GG|GH|GI|GL|GM|GN|GOV|GP|GQ|G"
		"R|GS|GT|GU|GW|GY|HK|HM|HN|HR|HT|HU|ID|IE|IL|IM|IN|INFO|INT|IO"
		"|IQ|IR|IS|IT|JE|JM|JO|JOBS|JP|KE|KG|KH|KI|KM|KN|KP|KR|KW|KY|K"
		"Z|LA|LB|LC|LI|LK|LR|LS|LT|LU|LV|LY|MA|MC|MD|ME|MG|MH|MIL|MK|M"
		"L|MM|MN|MO|MOBI|MP|MQ|MR|MS|MT|MU|MUSEUM|MV|MW|MX|MY|MZ|NA|NA"
		"ME|NC|NE|NET|NF|NG|NI|NL|NO|NP|NR|NU|NZ|OM|ORG|PA|PE|PF|PG|PH"
		"|PK|PL|PM|PN|POST|PR|PRO|PS|PT|PW|PY|QA|RE|RO|RS|RU|RW|SA|SB|"
		"SC|SD|SE|SG|SH|SI|SJ|SK|SL|SM|SN|SO|SR|ST|SU|SV|SX|SY|SZ|TC|T"
		"D|TEL|TF|TG|TH|TJ|TK|TL|TM|TN|TO|TP|TR|TRAVEL|TT|TV|TW|TZ|UA|"
		"UG|UK|US|UY|UZ|VA|VC|VE|VG|VI|VN|VU|WF|WS|XN"
		"--0ZWM56D|XN--11B5BS3A9AJ6G|XN--3E0B707E|XN--45BRJ9C|XN--80AK"
		"HBYKNJ4F|XN--80AO21A|XN--90A3AC|XN--9T4B11YI5A|XN--CLCHC0EA0B"
		"2G2A9GCD|XN--DEBA0AD|XN--FIQS8S|XN--FIQZ9S|XN--FPCRJ9C3D|XN--"
		"FZC2C9E2C|XN--G6W251D|XN--GECRJ9C|XN--H2BRJ9C|XN--HGBK6AJ7F53"
		"BBA|XN--HLCJ6AYA9ESC7A|XN--J6W193G|XN--JXALPDLP|XN--KGBECHTV|"
		"XN--KPRW13D|XN--KPRY57D|XN--LGBBAT1AD8J|XN--MGB9AWBF|XN--MGBA"
		"AM7A8H|XN--MGBAYH7GPA|XN--MGBBH1A71E|XN--MGBC0A9AZCG|XN--MGBE"
		"RP4A5D4AR|XN--MGBX4CD0AB|XN--O3CW4H|XN--OGBPF8FL|XN--P1AI|XN-"
		"-PGBS0DH|XN--S9BRJ9C|XN--WGBH1C|XN--WGBL6A|XN--XKC2AL3HYE2A|X"
		"N--XKC2DL3A5EE0H|XN--YFRO4I67O|XN--YGBI2AMMX|XN--ZCKZAH|XXX|Y"
		"E|YT|ZA|ZM|ZW"
		"))|(?:[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})|(?:"
		"\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\]))$",
	case re:run(FQDN,DomRegex,[caseless, anchored]) of
	  {match, _Captured} ->
	    {true,FQDN};
	  matched ->
	    {true,FQDN};
	  nomatch ->
	    {false,FQDN}
	end.
	
