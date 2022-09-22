/* Hide email modal */
window.onclick = (event) => {
  const modal = document.getElementById("mailgo");
  if (event.target === modal) {
    modal.style.display = "none";
  }
};

const buildThresholdList = () => {
  let thresholds = [];
  let numSteps = 30;

  for (let i = 1.0; i <= numSteps; i++) {
    let ratio = i / numSteps;
    thresholds.push(ratio);
  }

  thresholds.push(0);
  return thresholds;
};

const loadHuella = (e) => {
  e.preventDefault();
  const loader = document.querySelector("#loader");

  if (loader) {
    loader.classList.add("show");

    const huellaForm = document.querySelectorAll(".slide");

    huellaForm.forEach((slide) => {
      slide.classList.add("hide");
    });

    setTimeout(() => {
      loader.classList.remove("show");
      const huellaSlides = document.querySelectorAll(".footprint");
      huellaSlides.forEach((slide) => {
        slide.classList.remove("hide");
      });
      location.hash = "#mi-huella-1";
    }, 3000);
  }
};

const startup = () => {
  const options = {
    root: null,
    rootMargin: "0px",
    threshold: buildThresholdList(),
  };

  const navbar = document.querySelector("#nav");

  const desktopCallback = (entries) => {
    entries.forEach((entry) => {
      if (window.innerWidth >= 992) {
        if (entry.intersectionRatio > 0.3) {
          navbar.classList.add("d-lg-none");
        } else {
          navbar.classList.remove("d-lg-none");
          navbar.classList.add("d-lg-block");
        }
      }
    });
  };

  const mobileCallback = (entries) => {
    entries.forEach((entry) => {
      if (window.innerWidth < 992) {
        if (entry.intersectionRatio > 0) {
          navbar.classList.remove("d-none");
        }
      }
    });
  };

  let el = document.getElementById("user-graf_sexo");
  el.parentElement.classList.add("col-md-2");
  el.parentElement.classList.add("col-6");
  el.parentElement.classList.add("col-sm-6");
  el.parentElement.classList.add("d-flex");
  el.parentElement.classList.add("justify-content-center");
  el.classList.add("bolita-boton-contra");
  el.classList.add("d-flex");
  el.classList.add("align-items-center");

  el = document.getElementById("user-graf_esc");
  el.parentElement.classList.add("col-md-4");
  el.parentElement.classList.add("col-6");
  el.parentElement.classList.add("col-sm-6");
  el.parentElement.classList.add("d-flex");
  el.parentElement.classList.add("justify-content-center");
  el.classList.add("bolita-boton-contra");
  el.classList.add("d-flex");
  el.classList.add("align-items-center");

  el = document.getElementById("user-graf_trab");
  el.parentElement.classList.add("col-md-4");
  el.parentElement.classList.add("col-6");
  el.parentElement.classList.add("col-sm-6");
  el.parentElement.classList.add("d-flex");
  el.parentElement.classList.add("justify-content-center");
  el.classList.add("bolita-boton-contra");
  el.classList.add("d-flex");
  el.classList.add("align-items-center");

  el = document.getElementById("user-graf_hij");
  el.parentElement.classList.add("col-md-2");
  el.parentElement.classList.add("col-6");
  el.parentElement.classList.add("col-sm-6");
  el.parentElement.classList.add("d-flex");
  el.parentElement.classList.add("justify-content-center");
  el.classList.add("bolita-boton-contra");
  el.classList.add("d-flex");
  el.classList.add("align-items-center");

  el = document.getElementById("user-distribucion");
  el.removeAttribute('style');
  el.classList.add("h-auto");

  el = document.getElementById("user-distribucion2");
  el.removeAttribute('style');
  el.classList.add("h-auto");

  el = document.getElementById("user-bar_ilust");
  el.removeAttribute('style');
  el.classList.add("tope");
 


  el = document.getElementById("user-muni_graf");
  el.classList.add("h-auto");

  el = document.getElementById("user-muni_graf_2");
  el.classList.add("h-auto");
  
  el = document.getElementById("user-contra_graf");
  el.removeAttribute('style');
  el.classList.add("h-auto");
  el.classList.add("w-100");

  // Get the modal
var modal = document.getElementById("myModal");

// Get the button that opens the modal
var btn = document.getElementById("myBtn");

// Get the <span> element that closes the modal
var span = document.getElementsByClassName("close")[0];

// When the user clicks the button, open the modal 
btn.onclick = function() {
  modal.style.display = "block";
}

// When the user clicks on <span> (x), close the modal
span.onclick = function() {
  modal.style.display = "none";
}

// When the user clicks anywhere outside of the modal, close it
window.onclick = function(event) {
  if (event.target == modal) {
    modal.style.display = "none";
  }
}

  
  el = document.getElementById("user-esc");
  el.parentElement.classList.add("triang");

  el = document.getElementById("user-ents");
  el.parentElement.classList.add("triang_ent");

  el = document.getElementById("user-muns");
  el.parentElement.classList.add("triang_mun");

  el = document.getElementById("user-select_scatter");
  el.parentElement.classList.add("triang");

  el = document.getElementById("user-select_scatter_2");
  el.parentElement.classList.add("triang");

  const desktopObserver = new IntersectionObserver(desktopCallback, options);
  const desktopTarget = document.querySelector("#initial-slide");

  const mobileObserver = new IntersectionObserver(mobileCallback, options);
  const mobileTarget = document.querySelector("#mi-huella-1");

  mobileObserver.observe(mobileTarget);
  desktopObserver.observe(desktopTarget);

  // Show huella event
  const huellaButton = document.querySelector("#huella-button-container");
  huellaButton.addEventListener("click", loadHuella);

  // Scroll observer, stops when it reaches #Ps-8 (before the results)
  const scrollObserver = new IntersectionObserver((entries) => {
    if (entries[0].isIntersecting === true) {
      const huellaSlides = document.querySelectorAll(".footprint");
      huellaSlides.forEach((slide) => {
        slide.classList.add("hide");
      });
    }
  });
  const scrollTarget = document.querySelector("#Ps-8");

  scrollObserver.observe(scrollTarget);
};

window.addEventListener("load", startup, false);

