let tables = document.getElementsByTagName("table");
for (let i = 0; i < tables.length; i++) {
    let table = tables[i];
    table.innerHTML = table.innerHTML.replace(/\(#tab:input rscript\)/g, "");
}
