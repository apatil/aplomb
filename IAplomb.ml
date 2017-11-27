let show ?context ?figuresPath ?figureName spec  =
  (*
    NOTE: Remote assets don't work in airgapped environments.

    However,
    - inlining them into the srcdoc results in a way-too-huge .ipynb file
    - local file URL's don't work in iframes
    - inlining the assets once into the notebook, then consuming them from multiple
      visualizations, could break other JS in the notebook

    Web!
  *)
  let s = ViewCommon.to_string ~assets:`Remote ?figureName spec in
  let content = "<iframe height=\"100%\" width=\"100%\" srcdoc='" ^ s ^ "' seamless frameborder=\"0\" style=\"overflow: hidden; height: 100%; width: 100%;\" scrolling=\"no\" onload=\"this.style.height=this.contentDocument.body.scrollHeight + 15 + 'px';\"></iframe>" in
  (* let content = "<iframe height=600 width=800 srcdoc='" ^ s ^ "'>/iframe>" in *)
  Iocaml.display ?context "text/html" content;
