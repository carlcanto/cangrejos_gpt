% =======================================================
% cangrejos.pl
% SOLUCION: Carga de librerias necesarias.
% =======================================================
:- use_module(library(strings)). % NECESARIO para string_lower/2, string_trim/3
:- use_module(library(clpfd)).    % NECESARIO para normalize_space

% Predicado principal que inicia la identificacion
diagnosticar(Especie) :-
    nl, write('--- INICIO DEL DIAGNOSTICO ---'), nl,
    
    % Nivel 1: Pregunta inicial
    obtener_respuesta('¿Observo el cangrejo de lejos? (si/no)', RespuestaAtom1),
    
    % Nivel 2/5: Bifurcacion de la logica
    nivel2_o_nivel5(RespuestaAtom1, Especie).

% Predicado auxiliar mejorado para solicitar y leer una respuesta (no requiere punto '.')
obtener_respuesta(Pregunta, RespuestaAtom) :-
    write(Pregunta), write(': '),
    flush_output,
    read_line_to_string(current_input, RespuestaStringRaw),
    string_lower(RespuestaStringRaw, RespuestaStringLower), % Convierte a minusculas
    
    % Limpieza de la cadena de entrada (elimina espacios y posibles puntos accidentales)
    normalize_space(string(RespuestaStringNormalized), RespuestaStringLower),
    
    % Convierte la cadena limpia en un atomo para la logica
    atom_string(RespuestaAtom, RespuestaStringNormalized).

% =======================================================
% NIVEL 1 & 2/5: Observacion General y Decision
% =======================================================

% Rama 'si' (Observacion de Lejos -> Identificacion por Habitat/Comportamiento)
nivel2_o_nivel5(si, Especie) :-
    nl, write('--- Modo: Identificacion por Habitat y Comportamiento ---'), nl,
    identificar_por_habitat(Especie).

% Rama 'no' (Observacion de Cerca -> Identificacion por Morfologia Detallada)
nivel2_o_nivel5(no, Especie) :-
    nl, write('--- Modo: Identificacion por Morfologia Detallada ---'), nl,
    identificar_por_morfologia(Especie).
    
% Manejo de respuesta invalida
nivel2_o_nivel5(Otra, _) :-
    nl, format('Error: Respuesta inicial invalida "~w". Debe ser "si" o "no".', [Otra]), nl,
    fail.

% =======================================================
% NIVEL 2: Habitat (Pregunta por Sustrato)
% =======================================================

identificar_por_habitat(Especie) :-
    obtener_respuesta('¿En que sustrato lo observo? (fai/afan/fae/are)', HabitatAtom),
    nl,
    identificar_segun_sustrato(HabitatAtom, Especie).

% --- Ramas del Habitat ---

% RAMA FAI (Fango Arenoso Intermarial) - CONTINUACION AL NIVEL 3
identificar_segun_sustrato(fai, Especie) :-
    write('Ubicacion: FAI (Fango Arenoso Intermarial).'), nl,
    rama_fai_por_tamano(Especie). 

% RAMAS PENDIENTES DE IMPLEMENTACION
identificar_segun_sustrato(afan, no_implementado) :-
    write('Ubicacion: AFAN. Esta rama aun no esta implementada.'), nl,
    fail. 
identificar_segun_sustrato(fae, no_implementado) :-
    write('Ubicacion: FAE. Esta rama aun no esta implementada.'), nl,
    fail.
identificar_segun_sustrato(are, no_implementado) :-
    write('Ubicacion: ARE. Esta rama aun no esta implementada.'), nl,
    fail.

% Manejo de habitat desconocido
identificar_segun_sustrato(Otro, _) :-
    format('Error: Habitat desconocido "~w".', [Otro]), nl,
    fail.

% =======================================================
% NIVEL 3: Tamano (INICIO de la Rama FAI)
% =======================================================

rama_fai_por_tamano(Especie) :-
    obtener_respuesta('¿Cual es el TAMANO del cangrejo? (muy_pequenio/pequenio/mediano)', TamanoAtom),
    nl,
    identificar_fai_segun_tamano(TamanoAtom, Especie).

% Sub-rama FAI: Tamano MUY PEQUENIO (C1)
identificar_fai_segun_tamano(muy_pequenio, Especie) :-
    write('Tamano: MUY PEQUENIO. Posibles especies: Leptuca batuenta, Leptuca inaequalis.'), nl,
    rama_fai_muy_pequenio_color(Especie). % <- Pasa al Nivel 4

% Sub-rama FAI: Tamano PEQUENIO (CN)
identificar_fai_segun_tamano(pequenio, Especie) :-
    write('Tamano: PEQUENIO. Posibles especies: Leptuca oesterdi, Leptuca stenodactylus.'), nl,
    rama_fai_pequenio_color(Especie). % <- Pasa al Nivel 4

% Sub-rama FAI: Tamano MEDIANO (OtroTamanio)
identificar_fai_segun_tamano(mediano, Especie) :-
    write('Tamano: MEDIANO. Posible especie: Leptuca umbratila.'), nl,
    rama_fai_mediano_color(Especie). % <- Pasa al Nivel 4

% Manejo de tamano desconocido
identificar_fai_segun_tamano(Otro, _) :-
    format('Error: Tamano invalido "~w".', [Otro]), nl,
    fail.

% =======================================================
% NIVEL 4: Color y Llamado (Continuacion de FAI)
% =======================================================

% --- RAMA FAI - MUY PEQUENIO (L. batuenta, L. inaequalis) ---

rama_fai_muy_pequenio_color(Especie) :-
    obtener_respuesta('Presenta Quela color blanca y extremidades marron-rojizo? (si/no)', Color1Atom),
    nl,
    identificar_batuenta_o_inaequalis(Color1Atom, Especie).

identificar_batuenta_o_inaequalis(si, Especie) :-
    obtener_respuesta('El Llamado consiste en movimientos rapidos de la quela golpeando el sustrato? (si/no)', Llamado1Atom),
    (
        Llamado1Atom == si
        -> Especie = l_batuenta, write('¡IDENTIFICADO! La especie es Leptuca batuenta.'), nl
        ;  Especie = no_determinada, write('No concuerda con L. batuenta. Continuando con L. inaequalis...'), nl,
           identificar_inaequalis_llamado(Especie)
    ).

identificar_batuenta_o_inaequalis(no, Especie) :-
    obtener_respuesta('Quela mayor gris-marron; dedos usualmente blancos con tinte naranja? (si/no)', Color2Atom),
    (
        Color2Atom == si
        -> write('Color de L. inaequalis detectado. Confirmando llamado...'), nl,
           identificar_inaequalis_llamado(Especie)
        ;  Especie = no_determinada, write('No concuerda con L. batuenta o L. inaequalis en esta rama.'), nl
    ).

identificar_inaequalis_llamado(Especie) :-
    obtener_respuesta('Llamado con extension lateral, movimiento con semiarco y vibrato (vibracion)? (si/no)', Llamado2Atom),
    (
        Llamado2Atom == si
        -> Especie = l_inaequalis, write('¡IDENTIFICADO! La especie es Leptuca inaequalis.'), nl
        ;  Especie = no_determinada, write('No se pudo determinar la especie.'), nl
    ).

% --- RAMA FAI - PEQUENIO (L. oesterdi, L. stenodactylus) ---

rama_fai_pequenio_color(Especie) :-
    obtener_respuesta('Presenta coloracion azul-aqua intenso en la parte frontal del caparazon? (si/no)', Color1Atom),
    nl,
    identificar_oesterdi_o_stenodactylus(Color1Atom, Especie).

identificar_oesterdi_o_stenodactylus(si, Especie) :-
    obtener_respuesta('Llamado lateral, inicia con extension de quela bajo y luego se retrae en arco? (si/no)', Llamado_oesterdiAtom),
    (
        Llamado_oesterdiAtom == si
        -> Especie = l_oesterdi, write('¡IDENTIFICADO! La especie es Leptuca oesterdi.'), nl
        ;  Especie = no_determinada, write('No concuerda con L. oesterdi.'), nl
    ).

identificar_oesterdi_o_stenodactylus(no, Especie) :-
    obtener_respuesta('Presenta caparazon azul y blanco, y Quela mayor rosa o blanca? (si/no)', Color2Atom),
    (
        Color2Atom == si
        -> obtener_respuesta('El cangrejo corre mientras hace llamado con su tenaza? (si/no)', Llamado_stenoAtom),
           (
               Llamado_stenoAtom == si
               -> Especie = l_stenodactylus, write('¡IDENTIFICADO! La especie es Leptuca stenodactylus.'), nl
               ;  Especie = no_determinada, write('No se pudo determinar L. stenodactylus.'), nl
           )
        ;  Especie = no_determinada, write('No concuerda con L. oesterdi o L. stenodactylus.'), nl
    ).

% --- RAMA FAI - MEDIANO (L. umbratila) ---

rama_fai_mediano_color(Especie) :-
    obtener_respuesta('Presenta dedos amarillentos en la quela mayor (color)? (si/no)', Color1Atom),
    (
        Color1Atom == si
        -> obtener_respuesta('Es una especie con baja actividad (llamado)? (si/no)', Llamado1Atom),
           (
               Llamado1Atom == si
               -> Especie = l_umbratila, write('¡IDENTIFICADO! La especie es Leptuca umbratila.'), nl
               ;  Especie = no_determinada, write('No concuerda con L. umbratila.'), nl
           )
        ;  Especie = no_determinada, write('No concuerda con L. umbratila.'), nl
    ).

% =======================================================
% RAMA DE MORFOLOGIA (NIVEL 5) - AUN NO IMPLEMENTADA
% =======================================================
identificar_por_morfologia(no_implementado) :-
    write('La rama de identificacion de cerca (morfologia) aun no esta implementada.'), nl.