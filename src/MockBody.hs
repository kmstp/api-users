{-# LANGUAGE QuasiQuotes #-}
module MockBody(mockBody) where

import           Text.RawString.QQ

mockBody = [r|<table class="ds-includeSet-table detailtable table table-striped table-hover">

<tr class="ds-table-row odd ">
<td class="label-cell">dc.contributor.editor</td>
<td class="word-break">McCurdy, Howard E.</td>
<td>en_us</td>
</tr>

<tr class="ds-table-row even ">
<td class="label-cell">dc.contributor.editor</td>
<td class="word-break">Launius, Roger D.</td>
<td>en_us</td>
</tr>
</table>
|]
