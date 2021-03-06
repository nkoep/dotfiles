$pdf_previewer = "start evince";
$pdf_update_method = 0;
$latex = 'latex  %O  --shell-escape %S';
$pdflatex = 'pdflatex  %O  --shell-escape %S';

add_cus_dep('glo', 'gls', 0, 'run_makeglossaries');
add_cus_dep('slo', 'sls', 0, 'run_makeglossaries');
add_cus_dep('acn', 'acr', 0, 'run_makeglossaries');

# Invoke makeglossaries automatically (https://tex.stackexchange.com/a/44316)
sub run_makeglossaries {
  if ( $silent ) {
    system "makeglossaries -q '$_[0]'";
  }
  else {
    system "makeglossaries '$_[0]'";
  };
}

push @generated_exts, 'glo', 'gls', 'glg';
push @generated_exts, 'slo', 'sls', 'slg';
push @generated_exts, 'acn', 'acr', 'alg';
$clean_ext .= ' %R.ist %R.xdy';
