
class PontuXLSession {
  session = pl.create(100000);
  constructor () {
    this.response = "";

    const resultParsing = this.session.consult (PONTUXL);

    if (resultParsing !== true) {
      console.error(pl.format_answer(resultParsing));
    }

    this.session.set_current_output(new pl.type.Stream(
      {
        put (text, _) {
          this.response += text;
          return true;
        },
        flush: () => true
      },
      'write', 'html_output', 'text', false, 'eof_code'
    ));
  }

  query(code, callback) {
  console.log(`?- ${code}`);
  this.session.query(code);
  this.session.answer(rep => {
    if (rep === false || rep === null || rep.id === "throw") {
      console.warn("❌ Pas de réponse valide");
      callback(false, null); // Échec
    } else {
      console.log("✅ Réponse Prolog :", pl.format_answer(rep));
      const result = rep.lookup("Message"); // ou "Message" selon le nom dans ta requête
      callback(true, result);
    }
  });
}

  
  reset_response() {
    this.response = '';
  }

  get_response() {
    console.log("Essai de retour de response")
    console.log(this.response)
    return this.response }
}
