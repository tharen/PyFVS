:: Run all keyword files against the official FVS binaries

set fvsroot=c:\FVSbin

call %fvsroot%\fvspn.exe --keywordfile=pn_bareground.key
copy pn_bareground.sum pn_bareground.sum.save

call %fvsroot%\fvswc.exe --keywordfile=wc_bareground.key
copy wc_bareground.sum wc_bareground.sum.save

call %fvsroot%\fvsso.exe --keywordfile=so_bareground.key
copy so_bareground.sum so_bareground.sum.save

call %fvsroot%\fvsca.exe --keywordfile=ca_bareground.key
copy ca_bareground.sum ca_bareground.sum.save
