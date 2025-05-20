// === Initialisation de l'état du jeu ===
let jeuCommence = false;

// === SESSIONS PROLOG ===
// Création d'une session Prolog avec PontuXL, utilisée pour le bot, le jeu et l'IA
const plSession = new PontuXLSession(); // Session Prolog
const game = plSession;        // Alias pour la session de jeu
const ia = game;               // Alias pour l'IA

// === Équipe qui commence le tour ===
let tourEquipeActuelle = "vert";

// === RECONNAISSANCE VOCALE DU BOT ===

// Sélection de l'élément HTML où afficher les textes reconnus et les réponses
const texts = document.querySelector(".texts");

// Compatibilité entre les navigateurs pour la reconnaissance vocale
window.SpeechRecognition =
  window.SpeechRecognition || window.webkitSpeechRecognition;

// Création d'une instance de reconnaissance vocale
const recognition = new SpeechRecognition();
recognition.interimResults = true; 
recognition.lang = 'fr-FR';        

// Préparation de la synthèse vocale pour faire parler le bot
const speech = new SpeechSynthesisUtterance();
speech.lang = "fr-FR";
speech.volume = 1; 
speech.rate = 1;   
speech.pitch = 1;  

// Variables globales pour stocker la question et la réponse
var question = '';
var realresponse = '';
var msg;

// Création d'un paragraphe pour afficher le texte (non utilisé ici directement)
let p = document.createElement("p");

// Événement déclenché lorsqu'un résultat de reconnaissance est disponible
recognition.addEventListener("result", (e) => {
  // Crée un nouveau paragraphe pour chaque résultat
  const p = document.createElement("p");
  texts.appendChild(p);

  // Extraction du texte reconnu à partir des résultats
  const text = Array.from(e.results)
    .map((result) => result[0])         
    .map((result) => result.transcript) 
    .join("");                          

  // Affichage du texte reconnu dans l'interface
  p.innerText = text;
  console.log(text);

  // Si la reconnaissance est terminée (résultat final)
  if (e.results[0].isFinal) {
    console.log("text prefix");
    console.log(text.slice(0, 20));

    const lowerText = text.toLowerCase(); 

    // Si le texte commence par "bot" ou "pontu", le bot doit répondre
    if (lowerText.slice(0, 15).includes("bot") || lowerText.slice(0, 20).includes("pontu")) {
      
      // Convertit la question en tableau pour Prolog
      const question = toArray(lowerText);

      // Envoie la question au moteur Prolog et attend une réponse
      plSession.query(`
        lire_question([${question}], L_Mots), 
        produire_reponse(L_Mots, L_reponse),
        transformer_reponse_en_string(L_reponse, Message).
      `, (repp, action) => {
        // Si aucune réponse, log dans la console
        if (!repp) return console.log("non");

        // Affiche la réponse brute dans la console
        console.log(action);

        // Transforme la réponse Prolog en texte lisible
        const realresponse = fromArrayCodeToString(jmjCodeToString(action));
        console.log(realresponse);

        // Crée un paragraphe pour afficher la réponse du bot
        const responseParagraph = document.createElement("p");
        responseParagraph.classList.add("replay"); 
        responseParagraph.innerText = realresponse;
        texts.appendChild(responseParagraph);

        // Configure et fait parler le bot
        speech.text = realresponse;
        window.speechSynthesis.speak(speech);
      });
    }
  }
});

// Quand la reconnaissance s'arrête (fin naturelle), elle redémarre automatiquement
recognition.addEventListener("end", () => {
  recognition.start();
});

// Lancement initial de la reconnaissance vocale
recognition.start();



// === BOT : TEXT INPUT ===
document.addEventListener('DOMContentLoaded', () => {
  const textQuestionForm = document.getElementById('text-question-form');

  textQuestionForm.addEventListener('submit', (event) => {
    event.preventDefault();
    let questionText = document.getElementById('text-question').value.trim();
    // Convertissez la question en tableau de codes de caractères
  const questionArray = toArray(questionText.toLowerCase());

   // Faites la requête Prolog avec la question
  plSession.query(`
    lire_question([${questionArray}], L_Mots), 
    produire_reponse(L_Mots, L_reponse),
    
    transformer_reponse_en_string(L_reponse, Message).
    `, (repp, action) => {
      console.log(action);

      if (!repp) return console.log("non");
    
      
      // Obtenez la réponse de Prolog
      
      const realResponse = fromArrayCodeToString(jmjCodeToString(action));
    
    
      displayResponse(realResponse);
    });
    });

  });
    
  
// === Affiche une réponse dans l'interface et la lit à voix haute ===
function displayResponse(response) {
  const p = document.createElement("p");   
  p.classList.add("replay");               
  p.innerText = response;                 
  texts.appendChild(p);                    

  speech.text = response;                  
  window.speechSynthesis.speak(speech);    
}

// === Affiche un message de bienvenue et fait disparaître une annonce ===
function annonceOut() {
  document.getElementById("annonce").style.display = "none"; 

  const mess_annonce = "Bienvenue ! Je suis le bot de PontuXL. Puis-je vous aider ?";

  // Crée et affiche le message de bienvenue
  p = document.createElement("p");
  p.classList.add("replay");
  p.innerText = mess_annonce;
  texts.appendChild(p);

  // Prépare et lit vocalement le message
  p = document.createElement("p");
  speech.text = mess_annonce;
  window.speechSynthesis.speak(speech);
} 

// === UTILITAIRES ===

// Convertit une chaîne de caractères en tableau de codes ASCII
function toArray(str) {
  const array = [];
  for (let i = 0; i < str.length; ++i) {
    array.push(str.charCodeAt(i)); 
  }
  array.push(10);
  return array;
}

// Convertit un tableau de codes ASCII en une chaîne de caractères
function fromArrayCodeToString(arr) {
  var res = [];
  for (var i = 0; i < arr.length; i++) {
    res.push(String.fromCharCode(arr[i])); 
  }
  return res.join(""); 
}

// Récupère récursivement les valeurs d'une structure Prolog (JMJ format) sous forme de tableau
function jmjCodeToString(parr) {
  if (parr.args.length == 0) {
    return []; 
  } else {
    const arr = jmjCodeToString(parr.args[1]); 
    arr.unshift(parr.args[0].value);           
    return arr;                                
  }
}


// Création du plateau


function createBoard() {
  const board = document.getElementById("game-board");
  board.innerHTML = "";

  for (let i = 0; i < 11; i++) {      // lignes
    for (let j = 0; j < 11; j++) {    // colonnes
      const cell = document.createElement("div");

      if (i % 2 === 0 && j % 2 === 0) {
        // Case noeud
        cell.classList.add("node");
        
        const logiqueX = j / 2;
        const logiqueY = 5 - i / 2; // Inversion de Y pour repère cartésien
        cell.dataset.coords = `${logiqueX}-${logiqueY}`;

        // Label de debug (optionnel)
        const label = document.createElement("span");
        label.innerText = `(${logiqueX},${logiqueY})`;
        label.style.fontSize = "8px";
        label.style.color = "#999";
        label.style.position = "absolute";
        cell.appendChild(label);

      } else if (i % 2 === 0 && j % 2 === 1) {
        // Pont horizontal
        cell.classList.add("bridge-horizontal");
        const x1 = Math.floor(j / 2);
        const x2 = x1 + 1;
        const y = 5 - (i / 2);
        cell.dataset.pont = `${x1}-${y}-${x2}-${y}`;

      } else if (i % 2 === 1 && j % 2 === 0) {
        // Pont vertical
        cell.classList.add("bridge-vertical");
        const x = j / 2;
        const y1 = 5 - Math.floor(i / 2);
        const y2 = y1 - 1;
        cell.dataset.pont = `${x}-${y1}-${x}-${y2}`;

      } else {
        // Case vide (entre les ponts)
        cell.style.width = "100%";
        cell.style.height = "100%";
      }

      board.appendChild(cell);
    }
  }
}
createBoard();

// Debut du jeu


// === Fonction pour démarrer le jeu ===
function startGame() {
  createBoard(); 
  // Affiche la fenêtre modale d'instructions
  const modal = document.getElementById("instructions-modal");
  modal.style.display = "flex";

  // Envoie une requête Prolog pour :
  // - Effacer tous les lutins existants
  // - Effacer tous les ponts existants
  // - Initialiser les nouveaux ponts
  game.query("retractall(lutin(_,_,_)), retractall(pont(_,_,_,_)), initialiser_ponts.", (res) => {
    console.log("Réinitialisation :", res); 
  });
}

// === Fonction pour fermer la modale et commencer réellement la partie ===
function closeModal() {
  // Cache la fenêtre modale
  const modal = document.getElementById("instructions-modal");
  modal.style.display = "none";

  // Cache le bouton de démarrage
  document.getElementById("start-button").style.display = "none";

  // Crée un message indiquant à qui c'est le tour de jouer
  const tourAffichage = document.createElement("p");
  tourAffichage.id = "current-turn"; 
  tourAffichage.innerText = "C'EST AU TOUR DE L'EQUIPE VERT DE JOUER";
  tourAffichage.style.fontWeight = "bold";      
  tourAffichage.style.marginBottom = "15px";     
  tourAffichage.style.color = "green";          

  // Ajoute ce message tout en haut de la zone des commandes
  const commandBox = document.querySelector(".command-box");
  commandBox.insertBefore(tourAffichage, commandBox.firstChild);

  // Active le flag indiquant que le jeu est en cours
  jeuCommence = true;
}





// === GESTION DES COMMANDES UTILISATEUR ===

// === UTILS ===

// 🔁 Réinitialise le champ de commande après chaque action
function resetInput() {
  document.getElementById("player-command").value = "";
}

// 🧠 Calcule les nouvelles coordonnées du pont après rotation
function calculerNouveauPont(x1, y1, x2, y2, ax, ay, direction) {
  let NewX1, NewY1, NewX2, NewY2;

  // 💡 Tu pourrais simplifier cette logique avec une table de transformation ou switch
  if (direction === "gauche" && x1 === x2) {
    NewX1 = ax - 1; NewY1 = ay;
    NewX2 = ax;     NewY2 = ay;
  } else if (direction === "droite" && x1 === x2) {
    NewX1 = ax;     NewY1 = ay;
    NewX2 = ax + 1; NewY2 = ay;
  } else if (direction === "gauche" && y1 === y2) {
    NewX1 = ax;     NewY1 = ay;
    NewX2 = ax;     NewY2 = ay + 1;
  } else if (direction === "droite" && y1 === y2) {
    NewX1 = ax;     NewY1 = ay - 1;
    NewX2 = ax;     NewY2 = ay;
  }

  return [NewX1, NewY1, NewX2, NewY2];
}

// === ACTIONS ===

// ✅ Place un lutin si la case est libre
function actionPlacer(equipe, numero, x, y) {
  game.query(`case_libre(${x}, ${y}).`, (libre) => {
    if (!libre) return afficherErreurCommande("La case est déjà occupée.");

    game.query(`placer_lutin('${equipe}', ${numero}, ${x}, ${y}).`, (res) => {
      if (res) {
        afficherLutinDansHTML(x, y, equipe, numero);
        passerAuJoueurSuivant();
      } else {
        afficherErreurCommande("Impossible de placer ce lutin. Il est peut-être déjà placé.");
      }
    });
  });
}

// 🔁 Déplacement avec vérification de pont et liberté de la case

function actionDeplacer(equipe, numero, x, y, callback) {
  game.query(`lutin(l('${equipe}', ${numero}), _, (X0, Y0)), \\+ pont_existe(X0, Y0, ${x}, ${y}).`, (noBridge) => {
    if (noBridge) return afficherErreurCommande("Aucun pont ne relie ces cases.");

    game.query(`case_libre(${x}, ${y}).`, (libre) => {
      if (!libre) return afficherErreurCommande("La case est déjà occupée.");

      if (callback) callback(); // 🧩 Le callback permet de combiner avec une autre action comme retirer/roter
    });
  });
}

// ❌ Retirer un pont si valide
function actionRetirerPont(x1, y1, x2, y2) {
  game.query(`pont_existe(${x1},${y1},${x2},${y2}).`, (pontExiste) => {
    if (!pontExiste) return afficherErreurCommande("Ce pont n'est plus valide.");

    game.query(`retirer_pont((x1,y1)-(x2,y2)).`, (res) => {
      if (res) {
        retirerPontDansHTML(x1, y1, x2, y2);
        passerAuJoueurSuivant();
      } else {
        afficherErreurCommande("Erreur lors du retrait du pont.");
      }
    });
  });
}

// 🔁 Rotation d’un pont autour d’un point pivot
function actionRoter(direction, x1, y1, x2, y2, ax, ay) {
  game.query(`pont_existe(${x1},${y1},${x2},${y2}).`, (pontExiste) => {
    if (!pontExiste) return afficherErreurCommande("Ce pont a déjà été retiré.");

    game.query(`roter_pont(${direction}, ${x1}, ${y1}, ${x2}, ${y2}, ${ax}, ${ay}).`, (res) => {
      if (res) {
        retirerPontDansHTML(x1, y1, x2, y2);
        const [nx1, ny1, nx2, ny2] = calculerNouveauPont(x1, y1, x2, y2, ax, ay, direction);
        afficherPontDansHTML(nx1, ny1, nx2, ny2);
        passerAuJoueurSuivant();
      } else {
        afficherErreurCommande("Rotation impossible.");
      }
    });
  });
}

// 🔁 Déplacement + retrait
function actionDeplacerEtRetirer(equipe, numero, x, y, x1, y1, x2, y2) {
  game.query(`tous_les_lutins_places('${equipe}').`, (ok) => {
    if (!ok) return afficherErreurCommande("Tous les lutins doivent être placés.");

    game.query(`pont_existe(${x1},${y1},${x2},${y2}).`, (pontExiste) => {
      if (!pontExiste) return afficherErreurCommande("Pont invalide.");

      actionDeplacer(equipe, numero, x, y, () => {
        game.query(`deplacer_et_retirer('${equipe}', ${numero}, ${x}, ${y}, pont(${x1},${y1},${x2},${y2})).`, (res) => {
          if (res) {
            removeLutinFromBoard(equipe, numero);
            afficherLutinDansHTML(x, y, equipe, numero);
            retirerPontDansHTML(x1, y1, x2, y2);
            passerAuJoueurSuivant();
          } else {
            afficherErreurCommande("Erreur lors du déplacement + retrait.");
          }
        });
      });
    });
  });
}

// 🔁 Déplacement + rotation

function actionDeplacerEtRoter(equipe, numero, x, y, x1, y1, x2, y2, direction, ax, ay) {
  game.query(`tous_les_lutins_places('${equipe}').`, (ok) => {
    if (!ok) return afficherErreurCommande("Tous les lutins doivent être placés.");

    game.query(`pont_existe(${x1},${y1},${x2},${y2}).`, (pontExiste) => {
      if (!pontExiste) return afficherErreurCommande("Pont invalide.");

      game.query(`case_de_rotation_correspondante(${x1},${y1},${x2},${y2},${ax},${ay}).`, (pivotOk) => {
        if (!pivotOk) return afficherErreurCommande("Case pivot incorrecte.");

        actionDeplacer(equipe, numero, x, y, () => {
          game.query(`deplacer_et_roter('${equipe}', ${numero}, ${direction}, ${x}, ${y}, ${x1}, ${y1}, ${x2}, ${y2}, ${ax}, ${ay}).`, (res) => {
            if (res) {
              removeLutinFromBoard(equipe, numero);
              afficherLutinDansHTML(x, y, equipe, numero);
              retirerPontDansHTML(x1, y1, x2, y2);
              const [nx1, ny1, nx2, ny2] = calculerNouveauPont(x1, y1, x2, y2, ax, ay, direction);
              afficherPontDansHTML(nx1, ny1, nx2, ny2);
              passerAuJoueurSuivant();
            } else {
              afficherErreurCommande("Erreur rotation.");
            }
          });
        });
      });
    });
  });
}

// === MAIN ===

// 🎯 Fonction principale qui lit la commande et appelle la bonne action
function traiterCommande() {
  const input = document.getElementById("player-command").value.trim().toLowerCase();
  const equipe = tourEquipeActuelle;

  if (!jeuCommence) {
    afficherErreurCommande("Veuillez d'abord cliquer sur 'Commencer'.");
    return;
  }

  const matchPlacer = input.match(/^placer\s+([1-4])\s+a\s+([0-5])-([0-5])$/);
  const matchDeplacerRetirer = input.match(/^deplacer\s+([1-4])\s+a\s+([0-5])-([0-5])\s+et\s+retirer\s+([0-5])-([0-5])-([0-5])-([0-5])$/);
  const matchDeplacerRoter = input.match(/^deplacer\s+([1-4])\s+a\s+([0-5])-([0-5])\s+et\s+roter\s+([0-5])-([0-5])-([0-5])-([0-5])\s+a\s+(gauche|droite)\s+autour\s+de\s+([0-5])-([0-5])$/);
  const matchRetirer = input.match(/^retirer\s+([0-5])-([0-5])-([0-5])-([0-5])$/);
  const matchRoter = input.match(/^roter\s+([0-5])-([0-5])-([0-5])-([0-5])\s+a\s+(gauche|droite)\s+autour\s+de\s+([0-5])-([0-5])$/);

  if (matchPlacer) {
    const [_, num, x, y] = matchPlacer;
    actionPlacer(equipe, num, x, y);
  } else if (matchDeplacerRetirer) {
    const [_, num, x, y, x1, y1, x2, y2] = matchDeplacerRetirer;
    actionDeplacerEtRetirer(equipe, num, x, y, x1, y1, x2, y2);
  } else if (matchDeplacerRoter) {
    const [_, num, x, y, x1, y1, x2, y2, dir, ax, ay] = matchDeplacerRoter;
    actionDeplacerEtRoter(equipe, num, x, y, x1, y1, x2, y2, dir, ax, ay);
  } else if (matchRetirer) {
    game.query(`equipe_bloquee_sans_etre_eliminee('${equipe}').`, (bloquee) => {
      if (!bloquee) {
        afficherErreurCommande("Vérifiez bien, il y a encore des lutins pouvant se déplacer.");
        return;
      }
      const [_, x1, y1, x2, y2] = matchRetirer;
      actionRetirerPont(x1, y1, x2, y2);
    });
  } else if (matchRoter) {
    game.query(`equipe_bloquee_sans_etre_eliminee('${equipe}').`, (bloquee) => {
      if (!bloquee) {
        afficherErreurCommande("Vérifiez bien, il y a encore des lutins pouvant se déplacer.");
        return;
      }
      const [_, x1, y1, x2, y2, dir, ax, ay] = matchRoter;
      actionRoter(dir, x1, y1, x2, y2, ax, ay);
    });
  } else {
    afficherErreurCommande("Commande invalide. Veuillez réessayer.");
  }

  resetInput();
}



//fonction ia 

function jouerTourIA(equipe) {
  ia.query(`alphabeta_ia('${equipe}', Message).`, (success, action) => {
    console.log("état actuel du", action);

    if (!success) {
      afficherErreurCommande("L'IA ne peut pas jouer.");
      return;
    }

    let commandeIA = "Commande inconnue";

    const functor = action.id;
    const args = action.args;

    if (functor === "placer") {
      const lutin = args[0].args; // l(Equipe, Num)
      const num = lutin[1].toString();
      const x = args[1].toString();
      const y = args[2].toString();
      commandeIA = `placer ${num} a ${x}-${y}`;
    }

    else if (functor === "deplacer_et_retirer") {
      const lutin = args[0].args;
      const num = lutin[1].toString();
      const x0 = args[3].toString();
      const y0 = args[4].toString();
      const pont = args[5].args.map(arg => arg.toString()).join("-");
      commandeIA = `deplacer ${num} a ${x0}-${y0} et retirer ${pont}`;
    }

    else if (functor === "deplacer_et_roter") {
      const lutin = args[0].args;
      const num = lutin[1].toString();
      const x0 = args[3].toString();
      const y0 = args[4].toString();
      const pont = args[5].args.map(arg => arg.toString()).join("-");
      const dir = args[6].toString();
      const ax = args[7].toString();
      const ay = args[8].toString();
      commandeIA = `deplacer ${num} a ${x0}-${y0} et roter ${pont} a ${dir} autour de ${ax}-${ay}`;
    }

    else if (functor === "retirer_pont") {
      const pont = args.map(arg => arg.toString()).join("-");
      commandeIA = `retirer ${pont}`;
    }

    else if (functor === "roter_pont") {
      const pont = args[0].args.map(arg => arg.toString()).join("-");
      const dir = args[1].toString();
      const ax = args[2].toString();
      const ay = args[3].toString();
      commandeIA = `roter ${pont} a ${dir} autour de ${ax}-${ay}`;
    }

    console.log(`Commande générée pour l'équipe ${equipe} :`, commandeIA);

    // ✅ Mettre la commande IA dans le champ
    document.getElementById("player-command").value = commandeIA;

    // Exécuter la commande après un court délai
    setTimeout(() => {
      traiterCommande();
    }, 1000);
  });
}
    


 function retirerPontDansHTML(x1, y1, x2, y2) {
  // Convertir coordonnées logiques en coordonnées HTML
  const htmlX1 = x1 * 2;
  const htmlY1 = (5 - y1) * 2; // Inversion Y
  const htmlX2 = x2 * 2;
  const htmlY2 = (5 - y2) * 2;

  // Trouver la cellule du pont entre les deux noeuds
  let pontCell;
  const board = document.getElementById("game-board");

  if (x1 === x2) { // Pont vertical
      const midY = Math.min(htmlY1, htmlY2) + 1;
      const index = midY * 11 + htmlX1;
      pontCell = board.children[index];
      if (pontCell) {
          pontCell.classList.remove("bridge-vertical");
          pontCell.style.backgroundColor = "transparent";
      }
  } else if (y1 === y2) { // Pont horizontal
      const midX = Math.min(htmlX1, htmlX2) + 1;
      const index = htmlY1 * 11 + midX;
      pontCell = board.children[index];
      if (pontCell) {
          pontCell.classList.remove("bridge-horizontal");
          pontCell.style.backgroundColor = "transparent";
      }
  }

  if (!pontCell) {
      console.error("Pont introuvable entre", `${x1}-${y1}`, "et", `${x2}-${y2}`);
  }
}

 //fonction pour retirer visuellement un pont

 function removeLutinFromBoard(equipe, numero) {
  const allLutins = document.querySelectorAll(`.lutin.${equipe}`);
  allLutins.forEach(lutin => {
    if (lutin.innerText.trim() === String(numero)) {
      lutin.remove();
    }
  });
}


function afficherLutinDansHTML(x, y, equipe, numero) {
  const htmlX = x * 2;
  const htmlY = (5 - y) * 2; // Inversion verticale (repère cartésien)

  const index = htmlY * 11 + htmlX;
  const cell = document.getElementById("game-board").children[index];

  if (!cell) {
    console.warn("⚠️ Cellule introuvable à l’index", index, `(x=${x}, y=${y})`);
    return;
  }

  const lutin = document.createElement("div");
  lutin.classList.add("lutin", equipe);
  lutin.innerText = numero;
  cell.appendChild(lutin);
}



function afficherPontDansHTML(x1, y1, x2, y2) {
  const board = document.getElementById("game-board");

  // Conversion en coordonnées HTML
  const htmlX1 = x1 * 2;
  const htmlY1 = (5 - y1) * 2; // Inversion verticale
  const htmlX2 = x2 * 2;
  const htmlY2 = (5 - y2) * 2;

  let pontCell = null;

  if (x1 === x2) {
    // Pont vertical
    const midY = Math.min(htmlY1, htmlY2) + 1;
    const index = midY * 11 + htmlX1;
    pontCell = board.children[index];
    if (pontCell) {
      pontCell.classList.add("bridge-vertical");
      pontCell.style.backgroundColor = "#888"; // couleur du pont
    }
  } else if (y1 === y2) {
    // Pont horizontal
    const midX = Math.min(htmlX1, htmlX2) + 1;
    const index = htmlY1 * 11 + midX;
    pontCell = board.children[index];
    if (pontCell) {
      pontCell.classList.add("bridge-horizontal");
      pontCell.style.backgroundColor = "#888"; // couleur du pont
    }
  }

  if (!pontCell) {
    console.error("❌ Pont introuvable entre", `${x1}-${y1}`, "et", `${x2}-${y2}`);
  }
}




// === TOUR DE JEU ===
function passerAuJoueurSuivant() {
  const ordre = ["vert", "bleue", "jaune", "rouge"];
  const index = ordre.indexOf(tourEquipeActuelle);
  const prochaineEquipe = ordre[(index + 1) % ordre.length];

  game.query(`equipe_eliminee('${prochaineEquipe}').`, (eliminee) => {
    if (eliminee) {
      afficherErreurCommande(`💀 Les "${prochaineEquipe}" sont éliminés !`);
      ordre.splice(index, 1); // Retirer de la rotation

      if (ordre.length === 1) {
        afficherErreurCommande(`🏆 L'équipe ${ordre[0].toUpperCase()} a gagné la partie !`);
        return;
      }

      passerAuJoueurSuivant(); // Relancer avec la prochaine
      return;
    }

    game.query(`equipe_bloquee_sans_etre_eliminee('${prochaineEquipe}').`, (bloquee) => {
      if (bloquee) {
        afficherErreurCommande(`⛔ Les "${prochaineEquipe}" ne peuvent pas bouger, mais peuvent retirer ou roter un pont.`);
      }

      // Mettre à jour le tour
      tourEquipeActuelle = prochaineEquipe;
      const tour = document.getElementById("current-turn");
      tour.innerText = `C'EST AU TOUR DE L'EQUIPE ${tourEquipeActuelle.toUpperCase()} DE JOUER`;
      tour.style.color = tourEquipeActuelle;

      // Si c'est au tour d'une IA, la faire jouer
      if (tourEquipeActuelle === "bleue" || tourEquipeActuelle === "rouge") {
        setTimeout(() => jouerTourIA(tourEquipeActuelle), 1000); // Délai pour la lisibilité
      }
    });
  });
}


// === MODALE ERREUR ===
function afficherErreurCommande(message = "Commande incorrecte, veuillez réessayer.") {
  const modal = document.getElementById("erreur-modal");
  modal.querySelector("p").innerText = message;
  modal.style.display = "flex";
}


function fermerErreur() {
  document.getElementById("erreur-modal").style.display = "none";
}



