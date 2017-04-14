/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId])
/******/ 			return installedModules[moduleId].exports;
/******/
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// identity function for calling harmony imports with the correct context
/******/ 	__webpack_require__.i = function(value) { return value; };
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, {
/******/ 				configurable: false,
/******/ 				enumerable: true,
/******/ 				get: getter
/******/ 			});
/******/ 		}
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 143);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, exports) {

var core = module.exports = {version: '2.4.0'};
if(typeof __e == 'number')__e = core; // eslint-disable-line no-undef

/***/ }),
/* 1 */
/***/ (function(module, exports, __webpack_require__) {

var store      = __webpack_require__(51)('wks')
  , uid        = __webpack_require__(38)
  , Symbol     = __webpack_require__(2).Symbol
  , USE_SYMBOL = typeof Symbol == 'function';

var $exports = module.exports = function(name){
  return store[name] || (store[name] =
    USE_SYMBOL && Symbol[name] || (USE_SYMBOL ? Symbol : uid)('Symbol.' + name));
};

$exports.store = store;

/***/ }),
/* 2 */
/***/ (function(module, exports) {

// https://github.com/zloirock/core-js/issues/86#issuecomment-115759028
var global = module.exports = typeof window != 'undefined' && window.Math == Math
  ? window : typeof self != 'undefined' && self.Math == Math ? self : Function('return this')();
if(typeof __g == 'number')__g = global; // eslint-disable-line no-undef

/***/ }),
/* 3 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_descriptor__ = __webpack_require__(91);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_descriptor___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_descriptor__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map__ = __webpack_require__(61);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_get_iterator__ = __webpack_require__(13);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_defineProperty__ = __webpack_require__(65);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_defineProperty___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_defineProperty__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_json_stringify__ = __webpack_require__(60);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_json_stringify___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_json_stringify__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_symbol_iterator__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_symbol_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_symbol_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_core_js_object_assign__ = __webpack_require__(90);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_core_js_object_assign___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_6_babel_runtime_core_js_object_assign__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_get_own_property_names__ = __webpack_require__(63);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_get_own_property_names___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_get_own_property_names__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of__ = __webpack_require__(92);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9_babel_runtime_helpers_typeof__ = __webpack_require__(66);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9_babel_runtime_helpers_typeof___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_9_babel_runtime_helpers_typeof__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_11_babel_runtime_helpers_createClass__ = __webpack_require__(7);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_11_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_11_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_12__Symbol__ = __webpack_require__(9);
/* unused harmony export NonDeclaredType */
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "g", function() { return Any; });
/* unused harmony export Unit */
/* harmony export (immutable) */ __webpack_exports__["n"] = Option;
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "h", function() { return FableArray; });
/* harmony export (immutable) */ __webpack_exports__["m"] = Tuple;
/* harmony export (immutable) */ __webpack_exports__["i"] = GenericParam;
/* unused harmony export Interface */
/* harmony export (immutable) */ __webpack_exports__["l"] = makeGeneric;
/* unused harmony export isGeneric */
/* unused harmony export getDefinition */
/* unused harmony export extendInfo */
/* unused harmony export hasInterface */
/* unused harmony export getPropertyNames */
/* unused harmony export isArray */
/* harmony export (immutable) */ __webpack_exports__["e"] = toString;
/* unused harmony export hash */
/* harmony export (immutable) */ __webpack_exports__["a"] = equals;
/* unused harmony export comparePrimitives */
/* harmony export (immutable) */ __webpack_exports__["d"] = compare;
/* harmony export (immutable) */ __webpack_exports__["j"] = equalsRecords;
/* harmony export (immutable) */ __webpack_exports__["k"] = compareRecords;
/* harmony export (immutable) */ __webpack_exports__["f"] = equalsUnions;
/* harmony export (immutable) */ __webpack_exports__["b"] = compareUnions;
/* unused harmony export createDisposable */
/* unused harmony export createObj */
/* unused harmony export toPlainJsObj */
/* unused harmony export round */
/* harmony export (immutable) */ __webpack_exports__["o"] = randomNext;
/* harmony export (immutable) */ __webpack_exports__["c"] = defaultArg;
/* unused harmony export applyOperator */













var NonDeclaredType = function () {
    function NonDeclaredType(kind, definition, generics) {
        __WEBPACK_IMPORTED_MODULE_10_babel_runtime_helpers_classCallCheck___default()(this, NonDeclaredType);

        this.kind = kind;
        this.definition = definition;
        this.generics = generics;
    }

    __WEBPACK_IMPORTED_MODULE_11_babel_runtime_helpers_createClass___default()(NonDeclaredType, [{
        key: "Equals",
        value: function Equals(other) {
            if (this.kind === other.kind && this.definition === other.definition) {
                return __WEBPACK_IMPORTED_MODULE_9_babel_runtime_helpers_typeof___default()(this.generics) === "object" ? equalsRecords(this.generics, other.generics) : this.generics === other.generics;
            }
            return false;
        }
    }]);

    return NonDeclaredType;
}();
var Any = new NonDeclaredType("Any");
var Unit = new NonDeclaredType("Unit");
function Option(t) {
    return new NonDeclaredType("Option", null, t);
}
function FableArray(t) {
    var isTypedArray = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

    var def = null,
        genArg = null;
    if (isTypedArray) {
        def = t;
    } else {
        genArg = t;
    }
    return new NonDeclaredType("Array", def, genArg);
}

function Tuple(ts) {
    return new NonDeclaredType("Tuple", null, ts);
}
function GenericParam(definition) {
    return new NonDeclaredType("GenericParam", definition);
}
function Interface(definition) {
    return new NonDeclaredType("Interface", definition);
}
function makeGeneric(typeDef, genArgs) {
    return new NonDeclaredType("GenericType", typeDef, genArgs);
}
function isGeneric(typ) {
    return typ instanceof NonDeclaredType && typ.kind === "GenericType";
}
function getDefinition(typ) {
    return isGeneric(typ) ? typ.definition : typ;
}
function extendInfo(cons, info) {
    var parent = __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of___default()(cons.prototype);
    if (typeof parent[__WEBPACK_IMPORTED_MODULE_12__Symbol__["a" /* default */].reflection] === "function") {
        var newInfo = {},
            parentInfo = parent[__WEBPACK_IMPORTED_MODULE_12__Symbol__["a" /* default */].reflection]();
        __WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_get_own_property_names___default()(info).forEach(function (k) {
            var i = info[k];
            if ((typeof i === "undefined" ? "undefined" : __WEBPACK_IMPORTED_MODULE_9_babel_runtime_helpers_typeof___default()(i)) === "object") {
                newInfo[k] = Array.isArray(i) ? (parentInfo[k] || []).concat(i) : __WEBPACK_IMPORTED_MODULE_6_babel_runtime_core_js_object_assign___default()(parentInfo[k] || {}, i);
            } else {
                newInfo[k] = i;
            }
        });
        return newInfo;
    }
    return info;
}
function hasInterface(obj, interfaceName) {
    if (interfaceName === "System.Collections.Generic.IEnumerable") {
        return typeof obj[__WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_symbol_iterator___default.a] === "function";
    } else if (typeof obj[__WEBPACK_IMPORTED_MODULE_12__Symbol__["a" /* default */].reflection] === "function") {
        var interfaces = obj[__WEBPACK_IMPORTED_MODULE_12__Symbol__["a" /* default */].reflection]().interfaces;
        return Array.isArray(interfaces) && interfaces.indexOf(interfaceName) > -1;
    }
    return false;
}
function getPropertyNames(obj) {
    if (obj == null) {
        return [];
    }
    var propertyMap = typeof obj[__WEBPACK_IMPORTED_MODULE_12__Symbol__["a" /* default */].reflection] === "function" ? obj[__WEBPACK_IMPORTED_MODULE_12__Symbol__["a" /* default */].reflection]().properties || [] : obj;
    return __WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_get_own_property_names___default()(propertyMap);
}
function isArray(obj) {
    return Array.isArray(obj) || ArrayBuffer.isView(obj);
}
function toString(o) {
    return o != null && typeof o.ToString == "function" ? o.ToString() : String(o);
}
function hash(x) {
    if (x != null && typeof x.GetHashCode == "function") {
        return x.GetHashCode();
    } else {
        var s = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_json_stringify___default()(x);
        var h = 5381,
            i = 0,
            len = s.length;
        while (i < len) {
            h = h * 33 ^ s.charCodeAt(i++);
        }
        return h;
    }
}
function equals(x, y) {
    if (x === y) return true;else if (x == null) return y == null;else if (y == null) return false;else if (__WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of___default()(x) !== __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of___default()(y)) return false;else if (typeof x.Equals === "function") return x.Equals(y);else if (Array.isArray(x)) {
        if (x.length != y.length) return false;
        for (var i = 0; i < x.length; i++) {
            if (!equals(x[i], y[i])) return false;
        }return true;
    } else if (ArrayBuffer.isView(x)) {
        if (x.byteLength !== y.byteLength) return false;
        var dv1 = new DataView(x.buffer),
            dv2 = new DataView(y.buffer);
        for (var _i = 0; _i < x.byteLength; _i++) {
            if (dv1.getUint8(_i) !== dv2.getUint8(_i)) return false;
        }return true;
    } else if (x instanceof Date) return x.getTime() === y.getTime();else return false;
}
function comparePrimitives(x, y) {
    return x === y ? 0 : x < y ? -1 : 1;
}
function compare(x, y) {
    if (x === y) return 0;
    if (x == null) return y == null ? 0 : -1;else if (y == null) return 1;else if (__WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of___default()(x) !== __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of___default()(y)) return -1;else if (typeof x.CompareTo === "function") return x.CompareTo(y);else if (Array.isArray(x)) {
        if (x.length != y.length) return x.length < y.length ? -1 : 1;
        for (var i = 0, j = 0; i < x.length; i++) {
            if ((j = compare(x[i], y[i])) !== 0) return j;
        }return 0;
    } else if (ArrayBuffer.isView(x)) {
        if (x.byteLength != y.byteLength) return x.byteLength < y.byteLength ? -1 : 1;
        var dv1 = new DataView(x.buffer),
            dv2 = new DataView(y.buffer);
        for (var _i2 = 0, b1 = 0, b2 = 0; _i2 < x.byteLength; _i2++) {
            b1 = dv1.getUint8(_i2), b2 = dv2.getUint8(_i2);
            if (b1 < b2) return -1;
            if (b1 > b2) return 1;
        }
        return 0;
    } else if (x instanceof Date) {
        var xtime = x.getTime(),
            ytime = y.getTime();
        return xtime === ytime ? 0 : xtime < ytime ? -1 : 1;
    } else if ((typeof x === "undefined" ? "undefined" : __WEBPACK_IMPORTED_MODULE_9_babel_runtime_helpers_typeof___default()(x)) === "object") {
        var xhash = hash(x),
            yhash = hash(y);
        if (xhash === yhash) {
            return equals(x, y) ? 0 : -1;
        } else {
            return xhash < yhash ? -1 : 1;
        }
    } else return x < y ? -1 : 1;
}
function equalsRecords(x, y) {
    if (x === y) {
        return true;
    } else {
        var keys = getPropertyNames(x);
        for (var i = 0; i < keys.length; i++) {
            if (!equals(x[keys[i]], y[keys[i]])) return false;
        }
        return true;
    }
}
function compareRecords(x, y) {
    if (x === y) {
        return 0;
    } else {
        var keys = getPropertyNames(x);
        for (var i = 0; i < keys.length; i++) {
            var res = compare(x[keys[i]], y[keys[i]]);
            if (res !== 0) return res;
        }
        return 0;
    }
}
function equalsUnions(x, y) {
    return x === y || x.tag === y.tag && equals(x.data, y.data);
}
function compareUnions(x, y) {
    if (x === y) {
        return 0;
    } else {
        var res = x.tag < y.tag ? -1 : x.tag > y.tag ? 1 : 0;
        return res !== 0 ? res : compare(x.data, y.data);
    }
}
function createDisposable(f) {
    return __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_defineProperty___default()({
        Dispose: f
    }, __WEBPACK_IMPORTED_MODULE_12__Symbol__["a" /* default */].reflection, function () {
        return { interfaces: ["System.IDisposable"] };
    });
}
var CaseRules = {
    None: 0,
    LowerFirst: 1
};
function createObj(fields) {
    var caseRule = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : CaseRules.None;

    var iter = __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_get_iterator___default()(fields);
    var cur = iter.next(),
        o = {},
        casesCache = null;
    while (!cur.done) {
        var value = cur.value;
        if (Array.isArray(value)) {
            o[value[0]] = value[1];
        } else {
            casesCache = casesCache || new __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map___default.a();
            var proto = __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of___default()(value);
            var cases = casesCache.get(proto),
                caseInfo = null;
            if (cases == null) {
                if (typeof proto[__WEBPACK_IMPORTED_MODULE_12__Symbol__["a" /* default */].reflection] === "function") {
                    cases = proto[__WEBPACK_IMPORTED_MODULE_12__Symbol__["a" /* default */].reflection]().cases;
                    casesCache.set(proto, cases);
                }
            }
            if (cases != null && Array.isArray(caseInfo = cases[value.tag])) {
                var key = caseInfo[0];
                if (caseRule === CaseRules.LowerFirst) {
                    key = key[0].toLowerCase() + key.substr(1);
                }
                o[key] = caseInfo.length === 1 ? true : value.data;
            } else {
                throw new Error("Cannot infer key and value of " + value);
            }
        }
        cur = iter.next();
    }
    return o;
}
function toPlainJsObj(source) {
    if (source != null && source.constructor != Object) {
        var target = {};
        var props = __WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_get_own_property_names___default()(source);
        for (var i = 0; i < props.length; i++) {
            target[props[i]] = source[props[i]];
        }
        var proto = __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of___default()(source);
        if (proto != null) {
            props = __WEBPACK_IMPORTED_MODULE_7_babel_runtime_core_js_object_get_own_property_names___default()(proto);
            for (var _i3 = 0; _i3 < props.length; _i3++) {
                var prop = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_descriptor___default()(proto, props[_i3]);
                if (prop.value) {
                    target[props[_i3]] = prop.value;
                } else if (prop.get) {
                    target[props[_i3]] = prop.get.apply(source);
                }
            }
        }
        return target;
    } else {
        return source;
    }
}
function round(value) {
    var digits = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;

    var m = Math.pow(10, digits);
    var n = +(digits ? value * m : value).toFixed(8);
    var i = Math.floor(n),
        f = n - i;
    var e = 1e-8;
    var r = f > 0.5 - e && f < 0.5 + e ? i % 2 == 0 ? i : i + 1 : Math.round(n);
    return digits ? r / m : r;
}
function randomNext(min, max) {
    return Math.floor(Math.random() * (max - min)) + min;
}
function defaultArg(arg, defaultValue, f) {
    return arg == null ? defaultValue : f != null ? f(arg) : arg;
}
function applyOperator(x, y, operator) {
    function getMethod(obj) {
        if ((typeof obj === "undefined" ? "undefined" : __WEBPACK_IMPORTED_MODULE_9_babel_runtime_helpers_typeof___default()(obj)) === "object") {
            var cons = __WEBPACK_IMPORTED_MODULE_8_babel_runtime_core_js_object_get_prototype_of___default()(obj).constructor;
            if (typeof cons[operator] === "function") {
                return cons[operator];
            }
        }
        return null;
    }
    var meth = getMethod(x);
    if (meth != null) {
        return meth(x, y);
    }
    meth = getMethod(y);
    if (meth != null) {
        return meth(x, y);
    }
    switch (operator) {
        case "op_Addition":
            return x + y;
        case "op_Subtraction":
            return x - y;
        case "op_Multiply":
            return x * y;
        case "op_Division":
            return x / y;
        case "op_Modulus":
            return x % y;
        case "op_LeftShift":
            return x << y;
        case "op_RightShift":
            return x >> y;
        case "op_BitwiseAnd":
            return x & y;
        case "op_BitwiseOr":
            return x | y;
        case "op_ExclusiveOr":
            return x ^ y;
        case "op_LogicalNot":
            return x + y;
        case "op_UnaryNegation":
            return !x;
        case "op_BooleanAnd":
            return x && y;
        case "op_BooleanOr":
            return x || y;
        default:
            return null;
    }
}

/***/ }),
/* 4 */
/***/ (function(module, exports, __webpack_require__) {

var anObject       = __webpack_require__(10)
  , IE8_DOM_DEFINE = __webpack_require__(68)
  , toPrimitive    = __webpack_require__(53)
  , dP             = Object.defineProperty;

exports.f = __webpack_require__(6) ? Object.defineProperty : function defineProperty(O, P, Attributes){
  anObject(O);
  P = toPrimitive(P, true);
  anObject(Attributes);
  if(IE8_DOM_DEFINE)try {
    return dP(O, P, Attributes);
  } catch(e){ /* empty */ }
  if('get' in Attributes || 'set' in Attributes)throw TypeError('Accessors not supported!');
  if('value' in Attributes)O[P] = Attributes.value;
  return O;
};

/***/ }),
/* 5 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

exports.default = function (instance, Constructor) {
  if (!(instance instanceof Constructor)) {
    throw new TypeError("Cannot call a class as a function");
  }
};

/***/ }),
/* 6 */
/***/ (function(module, exports, __webpack_require__) {

// Thank's IE8 for his funny defineProperty
module.exports = !__webpack_require__(15)(function(){
  return Object.defineProperty({}, 'a', {get: function(){ return 7; }}).a != 7;
});

/***/ }),
/* 7 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _defineProperty = __webpack_require__(62);

var _defineProperty2 = _interopRequireDefault(_defineProperty);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function () {
  function defineProperties(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor) descriptor.writable = true;
      (0, _defineProperty2.default)(target, descriptor.key, descriptor);
    }
  }

  return function (Constructor, protoProps, staticProps) {
    if (protoProps) defineProperties(Constructor.prototype, protoProps);
    if (staticProps) defineProperties(Constructor, staticProps);
    return Constructor;
  };
}();

/***/ }),
/* 8 */
/***/ (function(module, exports, __webpack_require__) {

var global    = __webpack_require__(2)
  , core      = __webpack_require__(0)
  , ctx       = __webpack_require__(14)
  , hide      = __webpack_require__(11)
  , PROTOTYPE = 'prototype';

var $export = function(type, name, source){
  var IS_FORCED = type & $export.F
    , IS_GLOBAL = type & $export.G
    , IS_STATIC = type & $export.S
    , IS_PROTO  = type & $export.P
    , IS_BIND   = type & $export.B
    , IS_WRAP   = type & $export.W
    , exports   = IS_GLOBAL ? core : core[name] || (core[name] = {})
    , expProto  = exports[PROTOTYPE]
    , target    = IS_GLOBAL ? global : IS_STATIC ? global[name] : (global[name] || {})[PROTOTYPE]
    , key, own, out;
  if(IS_GLOBAL)source = name;
  for(key in source){
    // contains in native
    own = !IS_FORCED && target && target[key] !== undefined;
    if(own && key in exports)continue;
    // export native or passed
    out = own ? target[key] : source[key];
    // prevent global pollution for namespaces
    exports[key] = IS_GLOBAL && typeof target[key] != 'function' ? source[key]
    // bind timers to global for call from export context
    : IS_BIND && own ? ctx(out, global)
    // wrap global constructors for prevent change them in library
    : IS_WRAP && target[key] == out ? (function(C){
      var F = function(a, b, c){
        if(this instanceof C){
          switch(arguments.length){
            case 0: return new C;
            case 1: return new C(a);
            case 2: return new C(a, b);
          } return new C(a, b, c);
        } return C.apply(this, arguments);
      };
      F[PROTOTYPE] = C[PROTOTYPE];
      return F;
    // make static versions for prototype methods
    })(out) : IS_PROTO && typeof out == 'function' ? ctx(Function.call, out) : out;
    // export proto methods to core.%CONSTRUCTOR%.methods.%NAME%
    if(IS_PROTO){
      (exports.virtual || (exports.virtual = {}))[key] = out;
      // export proto methods to core.%CONSTRUCTOR%.prototype.%NAME%
      if(type & $export.R && expProto && !expProto[key])hide(expProto, key, out);
    }
  }
};
// type bitmap
$export.F = 1;   // forced
$export.G = 2;   // global
$export.S = 4;   // static
$export.P = 8;   // proto
$export.B = 16;  // bind
$export.W = 32;  // wrap
$export.U = 64;  // safe
$export.R = 128; // real proto method for `library` 
module.exports = $export;

/***/ }),
/* 9 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol__ = __webpack_require__(64);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map__ = __webpack_require__(61);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map__);
/* harmony export (immutable) */ __webpack_exports__["b"] = setType;
/* unused harmony export getType */


var types = new __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_map___default.a();
function setType(fullName, cons) {
    types.set(fullName, cons);
}
function getType(fullName) {
    return types.get(fullName);
}
/* harmony default export */ __webpack_exports__["a"] = ({
    reflection: __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol___default()("reflection")
});

/***/ }),
/* 10 */
/***/ (function(module, exports, __webpack_require__) {

var isObject = __webpack_require__(17);
module.exports = function(it){
  if(!isObject(it))throw TypeError(it + ' is not an object!');
  return it;
};

/***/ }),
/* 11 */
/***/ (function(module, exports, __webpack_require__) {

var dP         = __webpack_require__(4)
  , createDesc = __webpack_require__(25);
module.exports = __webpack_require__(6) ? function(object, key, value){
  return dP.f(object, key, createDesc(1, value));
} : function(object, key, value){
  object[key] = value;
  return object;
};

/***/ }),
/* 12 */
/***/ (function(module, exports, __webpack_require__) {

// to indexed object, toObject with fallback for non-array-like ES3 strings
var IObject = __webpack_require__(43)
  , defined = __webpack_require__(33);
module.exports = function(it){
  return IObject(defined(it));
};

/***/ }),
/* 13 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(96), __esModule: true };

/***/ }),
/* 14 */
/***/ (function(module, exports, __webpack_require__) {

// optional / simple context binding
var aFunction = __webpack_require__(39);
module.exports = function(fn, that, length){
  aFunction(fn);
  if(that === undefined)return fn;
  switch(length){
    case 1: return function(a){
      return fn.call(that, a);
    };
    case 2: return function(a, b){
      return fn.call(that, a, b);
    };
    case 3: return function(a, b, c){
      return fn.call(that, a, b, c);
    };
  }
  return function(/* ...args */){
    return fn.apply(that, arguments);
  };
};

/***/ }),
/* 15 */
/***/ (function(module, exports) {

module.exports = function(exec){
  try {
    return !!exec();
  } catch(e){
    return true;
  }
};

/***/ }),
/* 16 */
/***/ (function(module, exports) {

var hasOwnProperty = {}.hasOwnProperty;
module.exports = function(it, key){
  return hasOwnProperty.call(it, key);
};

/***/ }),
/* 17 */
/***/ (function(module, exports) {

module.exports = function(it){
  return typeof it === 'object' ? it !== null : typeof it === 'function';
};

/***/ }),
/* 18 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__TimeSpan__ = __webpack_require__(88);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__Util__ = __webpack_require__(3);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Long__ = __webpack_require__(58);
/* unused harmony export minValue */
/* unused harmony export maxValue */
/* unused harmony export parse */
/* unused harmony export tryParse */
/* unused harmony export create */
/* unused harmony export now */
/* unused harmony export utcNow */
/* unused harmony export today */
/* unused harmony export isLeapYear */
/* unused harmony export daysInMonth */
/* unused harmony export toUniversalTime */
/* unused harmony export toLocalTime */
/* unused harmony export timeOfDay */
/* unused harmony export date */
/* unused harmony export kind */
/* harmony export (immutable) */ __webpack_exports__["e"] = day;
/* harmony export (immutable) */ __webpack_exports__["c"] = hour;
/* unused harmony export millisecond */
/* harmony export (immutable) */ __webpack_exports__["f"] = minute;
/* harmony export (immutable) */ __webpack_exports__["d"] = month;
/* harmony export (immutable) */ __webpack_exports__["a"] = second;
/* harmony export (immutable) */ __webpack_exports__["b"] = year;
/* unused harmony export dayOfWeek */
/* unused harmony export ticks */
/* unused harmony export toBinary */
/* unused harmony export dayOfYear */
/* unused harmony export add */
/* unused harmony export addDays */
/* unused harmony export addHours */
/* unused harmony export addMinutes */
/* unused harmony export addSeconds */
/* unused harmony export addMilliseconds */
/* unused harmony export addTicks */
/* unused harmony export addYears */
/* unused harmony export addMonths */
/* unused harmony export subtract */
/* unused harmony export toLongDateString */
/* unused harmony export toShortDateString */
/* unused harmony export toLongTimeString */
/* unused harmony export toShortTimeString */
/* unused harmony export equals */
/* unused harmony export compare */
/* unused harmony export compareTo */
/* unused harmony export op_Addition */
/* unused harmony export op_Subtraction */



function minValue() {
    return parse(-8640000000000000, 1);
}
function maxValue() {
    return parse(8640000000000000, 1);
}
function parse(v, kind) {
    if (kind == null) {
        kind = typeof v == "string" && v.slice(-1) == "Z" ? 1 : 2;
    }
    var date = v == null ? new Date() : new Date(v);
    if (kind === 2) {
        date.kind = kind;
    }
    if (isNaN(date.getTime())) {
        throw new Error("The string is not a valid Date.");
    }
    return date;
}
function tryParse(v) {
    try {
        return [true, parse(v)];
    } catch (_err) {
        return [false, minValue()];
    }
}
function create(year, month, day) {
    var h = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;
    var m = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;
    var s = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : 0;
    var ms = arguments.length > 6 && arguments[6] !== undefined ? arguments[6] : 0;
    var kind = arguments.length > 7 && arguments[7] !== undefined ? arguments[7] : 2;

    var date = void 0;
    if (kind === 2) {
        date = new Date(year, month - 1, day, h, m, s, ms);
        date.kind = kind;
    } else {
        date = new Date(Date.UTC(year, month - 1, day, h, m, s, ms));
    }
    if (isNaN(date.getTime())) {
        throw new Error("The parameters describe an unrepresentable Date.");
    }
    return date;
}
function now() {
    return parse();
}
function utcNow() {
    return parse(null, 1);
}
function today() {
    return date(now());
}
function isLeapYear(year) {
    return year % 4 == 0 && year % 100 != 0 || year % 400 == 0;
}
function daysInMonth(year, month) {
    return month == 2 ? isLeapYear(year) ? 29 : 28 : month >= 8 ? month % 2 == 0 ? 31 : 30 : month % 2 == 0 ? 30 : 31;
}
function toUniversalTime(d) {
    return d.kind === 2 ? new Date(d.getTime()) : d;
}
function toLocalTime(d) {
    if (d.kind === 2) {
        return d;
    } else {
        var d2 = new Date(d.getTime());
        d2.kind = 2;
        return d2;
    }
}
function timeOfDay(d) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_0__TimeSpan__["a" /* create */])(0, hour(d), minute(d), second(d), millisecond(d));
}
function date(d) {
    return create(year(d), month(d), day(d), 0, 0, 0, 0, d.kind || 1);
}
function kind(d) {
    return d.kind || 1;
}
function day(d) {
    return d.kind === 2 ? d.getDate() : d.getUTCDate();
}
function hour(d) {
    return d.kind === 2 ? d.getHours() : d.getUTCHours();
}
function millisecond(d) {
    return d.kind === 2 ? d.getMilliseconds() : d.getUTCMilliseconds();
}
function minute(d) {
    return d.kind === 2 ? d.getMinutes() : d.getUTCMinutes();
}
function month(d) {
    return (d.kind === 2 ? d.getMonth() : d.getUTCMonth()) + 1;
}
function second(d) {
    return d.kind === 2 ? d.getSeconds() : d.getUTCSeconds();
}
function year(d) {
    return d.kind === 2 ? d.getFullYear() : d.getUTCFullYear();
}
function dayOfWeek(d) {
    return d.kind === 2 ? d.getDay() : d.getUTCDay();
}
function ticks(d) {
    return __WEBPACK_IMPORTED_MODULE_2__Long__["a" /* fromNumber */](d.getTime()).add(62135596800000).sub(d.kind == 2 ? d.getTimezoneOffset() * 60 * 1000 : 0).mul(10000);
}
function toBinary(d) {
    return ticks(d);
}
function dayOfYear(d) {
    var _year = year(d);
    var _month = month(d);
    var _day = day(d);
    for (var i = 1; i < _month; i++) {
        _day += daysInMonth(_year, i);
    }return _day;
}
function add(d, ts) {
    return parse(d.getTime() + ts, d.kind || 1);
}
function addDays(d, v) {
    return parse(d.getTime() + v * 86400000, d.kind || 1);
}
function addHours(d, v) {
    return parse(d.getTime() + v * 3600000, d.kind || 1);
}
function addMinutes(d, v) {
    return parse(d.getTime() + v * 60000, d.kind || 1);
}
function addSeconds(d, v) {
    return parse(d.getTime() + v * 1000, d.kind || 1);
}
function addMilliseconds(d, v) {
    return parse(d.getTime() + v, d.kind || 1);
}
function addTicks(d, t) {
    return parse(__WEBPACK_IMPORTED_MODULE_2__Long__["a" /* fromNumber */](d.getTime()).add(t.div(10000)).toNumber(), d.kind || 1);
}
function addYears(d, v) {
    var newMonth = month(d);
    var newYear = year(d) + v;
    var _daysInMonth = daysInMonth(newYear, newMonth);
    var newDay = Math.min(_daysInMonth, day(d));
    return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), d.kind || 1);
}
function addMonths(d, v) {
    var newMonth = month(d) + v;
    var newMonth_ = 0;
    var yearOffset = 0;
    if (newMonth > 12) {
        newMonth_ = newMonth % 12;
        yearOffset = Math.floor(newMonth / 12);
        newMonth = newMonth_;
    } else if (newMonth < 1) {
        newMonth_ = 12 + newMonth % 12;
        yearOffset = Math.floor(newMonth / 12) + (newMonth_ == 12 ? -1 : 0);
        newMonth = newMonth_;
    }
    var newYear = year(d) + yearOffset;
    var _daysInMonth = daysInMonth(newYear, newMonth);
    var newDay = Math.min(_daysInMonth, day(d));
    return create(newYear, newMonth, newDay, hour(d), minute(d), second(d), millisecond(d), d.kind || 1);
}
function subtract(d, that) {
    return typeof that == "number" ? parse(d.getTime() - that, d.kind || 1) : d.getTime() - that.getTime();
}
function toLongDateString(d) {
    return d.toDateString();
}
function toShortDateString(d) {
    return d.toLocaleDateString();
}
function toLongTimeString(d) {
    return d.toLocaleTimeString();
}
function toShortTimeString(d) {
    return d.toLocaleTimeString().replace(/:\d\d(?!:)/, "");
}
function equals(d1, d2) {
    return d1.getTime() == d2.getTime();
}
function compare(x, y) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_1__Util__["d" /* compare */])(x, y);
}
function compareTo(x, y) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_1__Util__["d" /* compare */])(x, y);
}
function op_Addition(x, y) {
    return add(x, y);
}
function op_Subtraction(x, y) {
    return subtract(x, y);
}

/***/ }),
/* 19 */
/***/ (function(module, exports) {

module.exports = {};

/***/ }),
/* 20 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var $at  = __webpack_require__(126)(true);

// 21.1.3.27 String.prototype[@@iterator]()
__webpack_require__(44)(String, 'String', function(iterated){
  this._t = String(iterated); // target
  this._i = 0;                // next index
// 21.1.5.2.1 %StringIteratorPrototype%.next()
}, function(){
  var O     = this._t
    , index = this._i
    , point;
  if(index >= O.length)return {value: undefined, done: true};
  point = $at(O, index);
  this._i += point.length;
  return {value: point, done: false};
});

/***/ }),
/* 21 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator__ = __webpack_require__(13);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass__ = __webpack_require__(7);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "e", function() { return Trampoline; });
/* harmony export (immutable) */ __webpack_exports__["b"] = protectedCont;
/* harmony export (immutable) */ __webpack_exports__["c"] = protectedBind;
/* harmony export (immutable) */ __webpack_exports__["d"] = protectedReturn;
/* unused harmony export AsyncBuilder */
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return singleton; });



var Trampoline = function () {
    __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(Trampoline, null, [{
        key: "maxTrampolineCallCount",
        get: function get() {
            return 2000;
        }
    }]);

    function Trampoline() {
        __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, Trampoline);

        this.callCount = 0;
    }

    __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(Trampoline, [{
        key: "incrementAndCheck",
        value: function incrementAndCheck() {
            return this.callCount++ > Trampoline.maxTrampolineCallCount;
        }
    }, {
        key: "hijack",
        value: function hijack(f) {
            this.callCount = 0;
            setTimeout(f, 0);
        }
    }]);

    return Trampoline;
}();
function protectedCont(f) {
    return function (ctx) {
        if (ctx.cancelToken.isCancelled) ctx.onCancel("cancelled");else if (ctx.trampoline.incrementAndCheck()) ctx.trampoline.hijack(function () {
            try {
                f(ctx);
            } catch (err) {
                ctx.onError(err);
            }
        });else try {
            f(ctx);
        } catch (err) {
            ctx.onError(err);
        }
    };
}
function protectedBind(computation, binder) {
    return protectedCont(function (ctx) {
        computation({
            onSuccess: function onSuccess(x) {
                try {
                    binder(x)(ctx);
                } catch (ex) {
                    ctx.onError(ex);
                }
            },
            onError: ctx.onError,
            onCancel: ctx.onCancel,
            cancelToken: ctx.cancelToken,
            trampoline: ctx.trampoline
        });
    });
}
function protectedReturn(value) {
    return protectedCont(function (ctx) {
        return ctx.onSuccess(value);
    });
}
var AsyncBuilder = function () {
    function AsyncBuilder() {
        __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, AsyncBuilder);
    }

    __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(AsyncBuilder, [{
        key: "Bind",
        value: function Bind(computation, binder) {
            return protectedBind(computation, binder);
        }
    }, {
        key: "Combine",
        value: function Combine(computation1, computation2) {
            return this.Bind(computation1, function () {
                return computation2;
            });
        }
    }, {
        key: "Delay",
        value: function Delay(generator) {
            return protectedCont(function (ctx) {
                return generator()(ctx);
            });
        }
    }, {
        key: "For",
        value: function For(sequence, body) {
            var iter = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator___default()(sequence);
            var cur = iter.next();
            return this.While(function () {
                return !cur.done;
            }, this.Delay(function () {
                var res = body(cur.value);
                cur = iter.next();
                return res;
            }));
        }
    }, {
        key: "Return",
        value: function Return(value) {
            return protectedReturn(value);
        }
    }, {
        key: "ReturnFrom",
        value: function ReturnFrom(computation) {
            return computation;
        }
    }, {
        key: "TryFinally",
        value: function TryFinally(computation, compensation) {
            return protectedCont(function (ctx) {
                computation({
                    onSuccess: function onSuccess(x) {
                        compensation();
                        ctx.onSuccess(x);
                    },
                    onError: function onError(x) {
                        compensation();
                        ctx.onError(x);
                    },
                    onCancel: function onCancel(x) {
                        compensation();
                        ctx.onCancel(x);
                    },
                    cancelToken: ctx.cancelToken,
                    trampoline: ctx.trampoline
                });
            });
        }
    }, {
        key: "TryWith",
        value: function TryWith(computation, catchHandler) {
            return protectedCont(function (ctx) {
                computation({
                    onSuccess: ctx.onSuccess,
                    onCancel: ctx.onCancel,
                    cancelToken: ctx.cancelToken,
                    trampoline: ctx.trampoline,
                    onError: function onError(ex) {
                        try {
                            catchHandler(ex)(ctx);
                        } catch (ex2) {
                            ctx.onError(ex2);
                        }
                    }
                });
            });
        }
    }, {
        key: "Using",
        value: function Using(resource, binder) {
            return this.TryFinally(binder(resource), function () {
                return resource.Dispose();
            });
        }
    }, {
        key: "While",
        value: function While(guard, computation) {
            var _this = this;

            if (guard()) return this.Bind(computation, function () {
                return _this.While(guard, computation);
            });else return this.Return(void 0);
        }
    }, {
        key: "Zero",
        value: function Zero() {
            return protectedCont(function (ctx) {
                return ctx.onSuccess(void 0);
            });
        }
    }]);

    return AsyncBuilder;
}();
var singleton = new AsyncBuilder();

/***/ }),
/* 22 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(107), __esModule: true };

/***/ }),
/* 23 */
/***/ (function(module, exports) {

var toString = {}.toString;

module.exports = function(it){
  return toString.call(it).slice(8, -1);
};

/***/ }),
/* 24 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.14 / 15.2.3.14 Object.keys(O)
var $keys       = __webpack_require__(78)
  , enumBugKeys = __webpack_require__(42);

module.exports = Object.keys || function keys(O){
  return $keys(O, enumBugKeys);
};

/***/ }),
/* 25 */
/***/ (function(module, exports) {

module.exports = function(bitmap, value){
  return {
    enumerable  : !(bitmap & 1),
    configurable: !(bitmap & 2),
    writable    : !(bitmap & 4),
    value       : value
  };
};

/***/ }),
/* 26 */
/***/ (function(module, exports, __webpack_require__) {

var def = __webpack_require__(4).f
  , has = __webpack_require__(16)
  , TAG = __webpack_require__(1)('toStringTag');

module.exports = function(it, tag, stat){
  if(it && !has(it = stat ? it : it.prototype, TAG))def(it, TAG, {configurable: true, value: tag});
};

/***/ }),
/* 27 */
/***/ (function(module, exports, __webpack_require__) {

// 7.1.13 ToObject(argument)
var defined = __webpack_require__(33);
module.exports = function(it){
  return Object(defined(it));
};

/***/ }),
/* 28 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(131);
var global        = __webpack_require__(2)
  , hide          = __webpack_require__(11)
  , Iterators     = __webpack_require__(19)
  , TO_STRING_TAG = __webpack_require__(1)('toStringTag');

for(var collections = ['NodeList', 'DOMTokenList', 'MediaList', 'StyleSheetList', 'CSSRuleList'], i = 0; i < 5; i++){
  var NAME       = collections[i]
    , Collection = global[NAME]
    , proto      = Collection && Collection.prototype;
  if(proto && !proto[TO_STRING_TAG])hide(proto, TO_STRING_TAG, NAME);
  Iterators[NAME] = Iterators.Array;
}

/***/ }),
/* 29 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_promise__ = __webpack_require__(93);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_promise___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_promise__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__AsyncBuilder__ = __webpack_require__(21);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__Choice__ = __webpack_require__(30);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__Seq__ = __webpack_require__(59);
/* unused harmony export awaitPromise */
/* unused harmony export cancellationToken */
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "c", function() { return defaultCancellationToken; });
/* unused harmony export catchAsync */
/* harmony export (immutable) */ __webpack_exports__["d"] = fromContinuations;
/* unused harmony export ignore */
/* unused harmony export parallel */
/* harmony export (immutable) */ __webpack_exports__["a"] = sleep;
/* unused harmony export start */
/* harmony export (immutable) */ __webpack_exports__["b"] = startImmediate;
/* unused harmony export startWithContinuations */
/* unused harmony export startAsPromise */










var Async = function Async() {
    __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, Async);
};

/* unused harmony default export */ var _unused_webpack_default_export = (Async);

function emptyContinuation(x) {}
function awaitPromise(p) {
    return fromContinuations(function (conts) {
        return p.then(conts[0]).catch(function (err) {
            return (err == "cancelled" ? conts[2] : conts[1])(err);
        });
    });
}
function cancellationToken() {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__AsyncBuilder__["b" /* protectedCont */])(function (ctx) {
        return ctx.onSuccess(ctx.cancelToken);
    });
}
var defaultCancellationToken = { isCancelled: false };
function catchAsync(work) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__AsyncBuilder__["b" /* protectedCont */])(function (ctx) {
        work({
            onSuccess: function onSuccess(x) {
                return ctx.onSuccess(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__Choice__["b" /* choice1Of2 */])(x));
            },
            onError: function onError(ex) {
                return ctx.onSuccess(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__Choice__["c" /* choice2Of2 */])(ex));
            },
            onCancel: ctx.onCancel,
            cancelToken: ctx.cancelToken,
            trampoline: ctx.trampoline
        });
    });
}
function fromContinuations(f) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__AsyncBuilder__["b" /* protectedCont */])(function (ctx) {
        return f([ctx.onSuccess, ctx.onError, ctx.onCancel]);
    });
}
function ignore(computation) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__AsyncBuilder__["c" /* protectedBind */])(computation, function (x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__AsyncBuilder__["d" /* protectedReturn */])(void 0);
    });
}
function parallel(computations) {
    return awaitPromise(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_promise___default.a.all(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__Seq__["a" /* map */])(function (w) {
        return startAsPromise(w);
    }, computations)));
}
function sleep(millisecondsDueTime) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__AsyncBuilder__["b" /* protectedCont */])(function (ctx) {
        setTimeout(function () {
            return ctx.cancelToken.isCancelled ? ctx.onCancel("cancelled") : ctx.onSuccess(void 0);
        }, millisecondsDueTime);
    });
}
function start(computation, cancellationToken) {
    return startWithContinuations(computation, cancellationToken);
}
function startImmediate(computation, cancellationToken) {
    return start(computation, cancellationToken);
}
function startWithContinuations(computation, continuation, exceptionContinuation, cancellationContinuation, cancelToken) {
    if (typeof continuation !== "function") {
        cancelToken = continuation;
        continuation = null;
    }
    var trampoline = new __WEBPACK_IMPORTED_MODULE_2__AsyncBuilder__["e" /* Trampoline */]();
    computation({
        onSuccess: continuation ? continuation : emptyContinuation,
        onError: exceptionContinuation ? exceptionContinuation : emptyContinuation,
        onCancel: cancellationContinuation ? cancellationContinuation : emptyContinuation,
        cancelToken: cancelToken ? cancelToken : defaultCancellationToken,
        trampoline: trampoline
    });
}
function startAsPromise(computation, cancellationToken) {
    return new __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_promise___default.a(function (resolve, reject) {
        return startWithContinuations(computation, resolve, reject, reject, cancellationToken ? cancellationToken : defaultCancellationToken);
    });
}

/***/ }),
/* 30 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__ = __webpack_require__(7);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Symbol__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__Util__ = __webpack_require__(3);
/* harmony export (immutable) */ __webpack_exports__["b"] = choice1Of2;
/* harmony export (immutable) */ __webpack_exports__["c"] = choice2Of2;





function choice1Of2(v) {
    return new Choice(0, v);
}
function choice2Of2(v) {
    return new Choice(1, v);
}

var Choice = function () {
    function Choice(tag, data) {
        __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default()(this, Choice);

        this.tag = tag | 0;
        this.data = data;
    }

    __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default()(Choice, [{
        key: "Equals",
        value: function Equals(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__Util__["f" /* equalsUnions */])(this, other);
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__Util__["b" /* compareUnions */])(this, other);
        }
    }, {
        key: __WEBPACK_IMPORTED_MODULE_2__Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "Microsoft.FSharp.Core.FSharpChoice",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: [["Choice1Of2", __WEBPACK_IMPORTED_MODULE_3__Util__["g" /* Any */]], ["Choice2Of2", __WEBPACK_IMPORTED_MODULE_3__Util__["g" /* Any */]]]
            };
        }
    }, {
        key: "valueIfChoice1",
        get: function get() {
            return this.tag === 0 ? this.data : null;
        }
    }, {
        key: "valueIfChoice2",
        get: function get() {
            return this.tag === 1 ? this.data : null;
        }
    }]);

    return Choice;
}();

/* harmony default export */ __webpack_exports__["a"] = (Choice);

/***/ }),
/* 31 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(95), __esModule: true };

/***/ }),
/* 32 */
/***/ (function(module, exports, __webpack_require__) {

// getting tag from 19.1.3.6 Object.prototype.toString()
var cof = __webpack_require__(23)
  , TAG = __webpack_require__(1)('toStringTag')
  // ES3 wrong here
  , ARG = cof(function(){ return arguments; }()) == 'Arguments';

// fallback for IE11 Script Access Denied error
var tryGet = function(it, key){
  try {
    return it[key];
  } catch(e){ /* empty */ }
};

module.exports = function(it){
  var O, T, B;
  return it === undefined ? 'Undefined' : it === null ? 'Null'
    // @@toStringTag case
    : typeof (T = tryGet(O = Object(it), TAG)) == 'string' ? T
    // builtinTag case
    : ARG ? cof(O)
    // ES3 arguments fallback
    : (B = cof(O)) == 'Object' && typeof O.callee == 'function' ? 'Arguments' : B;
};

/***/ }),
/* 33 */
/***/ (function(module, exports) {

// 7.2.1 RequireObjectCoercible(argument)
module.exports = function(it){
  if(it == undefined)throw TypeError("Can't call method on  " + it);
  return it;
};

/***/ }),
/* 34 */
/***/ (function(module, exports, __webpack_require__) {

var ctx         = __webpack_require__(14)
  , call        = __webpack_require__(71)
  , isArrayIter = __webpack_require__(69)
  , anObject    = __webpack_require__(10)
  , toLength    = __webpack_require__(37)
  , getIterFn   = __webpack_require__(56)
  , BREAK       = {}
  , RETURN      = {};
var exports = module.exports = function(iterable, entries, fn, that, ITERATOR){
  var iterFn = ITERATOR ? function(){ return iterable; } : getIterFn(iterable)
    , f      = ctx(fn, that, entries ? 2 : 1)
    , index  = 0
    , length, step, iterator, result;
  if(typeof iterFn != 'function')throw TypeError(iterable + ' is not iterable!');
  // fast case for arrays with default iterator
  if(isArrayIter(iterFn))for(length = toLength(iterable.length); length > index; index++){
    result = entries ? f(anObject(step = iterable[index])[0], step[1]) : f(iterable[index]);
    if(result === BREAK || result === RETURN)return result;
  } else for(iterator = iterFn.call(iterable); !(step = iterator.next()).done; ){
    result = call(iterator, f, step.value, entries);
    if(result === BREAK || result === RETURN)return result;
  }
};
exports.BREAK  = BREAK;
exports.RETURN = RETURN;

/***/ }),
/* 35 */
/***/ (function(module, exports) {

module.exports = true;

/***/ }),
/* 36 */
/***/ (function(module, exports) {

exports.f = {}.propertyIsEnumerable;

/***/ }),
/* 37 */
/***/ (function(module, exports, __webpack_require__) {

// 7.1.15 ToLength
var toInteger = __webpack_require__(52)
  , min       = Math.min;
module.exports = function(it){
  return it > 0 ? min(toInteger(it), 0x1fffffffffffff) : 0; // pow(2, 53) - 1 == 9007199254740991
};

/***/ }),
/* 38 */
/***/ (function(module, exports) {

var id = 0
  , px = Math.random();
module.exports = function(key){
  return 'Symbol('.concat(key === undefined ? '' : key, ')_', (++id + px).toString(36));
};

/***/ }),
/* 39 */
/***/ (function(module, exports) {

module.exports = function(it){
  if(typeof it != 'function')throw TypeError(it + ' is not a function!');
  return it;
};

/***/ }),
/* 40 */
/***/ (function(module, exports) {

module.exports = function(it, Constructor, name, forbiddenField){
  if(!(it instanceof Constructor) || (forbiddenField !== undefined && forbiddenField in it)){
    throw TypeError(name + ': incorrect invocation!');
  } return it;
};

/***/ }),
/* 41 */
/***/ (function(module, exports, __webpack_require__) {

var isObject = __webpack_require__(17)
  , document = __webpack_require__(2).document
  // in old IE typeof document.createElement is 'object'
  , is = isObject(document) && isObject(document.createElement);
module.exports = function(it){
  return is ? document.createElement(it) : {};
};

/***/ }),
/* 42 */
/***/ (function(module, exports) {

// IE 8- don't enum bug keys
module.exports = (
  'constructor,hasOwnProperty,isPrototypeOf,propertyIsEnumerable,toLocaleString,toString,valueOf'
).split(',');

/***/ }),
/* 43 */
/***/ (function(module, exports, __webpack_require__) {

// fallback for non-array-like ES3 and non-enumerable old V8 strings
var cof = __webpack_require__(23);
module.exports = Object('z').propertyIsEnumerable(0) ? Object : function(it){
  return cof(it) == 'String' ? it.split('') : Object(it);
};

/***/ }),
/* 44 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var LIBRARY        = __webpack_require__(35)
  , $export        = __webpack_require__(8)
  , redefine       = __webpack_require__(79)
  , hide           = __webpack_require__(11)
  , has            = __webpack_require__(16)
  , Iterators      = __webpack_require__(19)
  , $iterCreate    = __webpack_require__(120)
  , setToStringTag = __webpack_require__(26)
  , getPrototypeOf = __webpack_require__(77)
  , ITERATOR       = __webpack_require__(1)('iterator')
  , BUGGY          = !([].keys && 'next' in [].keys()) // Safari has buggy iterators w/o `next`
  , FF_ITERATOR    = '@@iterator'
  , KEYS           = 'keys'
  , VALUES         = 'values';

var returnThis = function(){ return this; };

module.exports = function(Base, NAME, Constructor, next, DEFAULT, IS_SET, FORCED){
  $iterCreate(Constructor, NAME, next);
  var getMethod = function(kind){
    if(!BUGGY && kind in proto)return proto[kind];
    switch(kind){
      case KEYS: return function keys(){ return new Constructor(this, kind); };
      case VALUES: return function values(){ return new Constructor(this, kind); };
    } return function entries(){ return new Constructor(this, kind); };
  };
  var TAG        = NAME + ' Iterator'
    , DEF_VALUES = DEFAULT == VALUES
    , VALUES_BUG = false
    , proto      = Base.prototype
    , $native    = proto[ITERATOR] || proto[FF_ITERATOR] || DEFAULT && proto[DEFAULT]
    , $default   = $native || getMethod(DEFAULT)
    , $entries   = DEFAULT ? !DEF_VALUES ? $default : getMethod('entries') : undefined
    , $anyNative = NAME == 'Array' ? proto.entries || $native : $native
    , methods, key, IteratorPrototype;
  // Fix native
  if($anyNative){
    IteratorPrototype = getPrototypeOf($anyNative.call(new Base));
    if(IteratorPrototype !== Object.prototype){
      // Set @@toStringTag to native iterators
      setToStringTag(IteratorPrototype, TAG, true);
      // fix for some old engines
      if(!LIBRARY && !has(IteratorPrototype, ITERATOR))hide(IteratorPrototype, ITERATOR, returnThis);
    }
  }
  // fix Array#{values, @@iterator}.name in V8 / FF
  if(DEF_VALUES && $native && $native.name !== VALUES){
    VALUES_BUG = true;
    $default = function values(){ return $native.call(this); };
  }
  // Define iterator
  if((!LIBRARY || FORCED) && (BUGGY || VALUES_BUG || !proto[ITERATOR])){
    hide(proto, ITERATOR, $default);
  }
  // Plug for library
  Iterators[NAME] = $default;
  Iterators[TAG]  = returnThis;
  if(DEFAULT){
    methods = {
      values:  DEF_VALUES ? $default : getMethod(VALUES),
      keys:    IS_SET     ? $default : getMethod(KEYS),
      entries: $entries
    };
    if(FORCED)for(key in methods){
      if(!(key in proto))redefine(proto, key, methods[key]);
    } else $export($export.P + $export.F * (BUGGY || VALUES_BUG), NAME, methods);
  }
  return methods;
};

/***/ }),
/* 45 */
/***/ (function(module, exports, __webpack_require__) {

var META     = __webpack_require__(38)('meta')
  , isObject = __webpack_require__(17)
  , has      = __webpack_require__(16)
  , setDesc  = __webpack_require__(4).f
  , id       = 0;
var isExtensible = Object.isExtensible || function(){
  return true;
};
var FREEZE = !__webpack_require__(15)(function(){
  return isExtensible(Object.preventExtensions({}));
});
var setMeta = function(it){
  setDesc(it, META, {value: {
    i: 'O' + ++id, // object ID
    w: {}          // weak collections IDs
  }});
};
var fastKey = function(it, create){
  // return primitive with prefix
  if(!isObject(it))return typeof it == 'symbol' ? it : (typeof it == 'string' ? 'S' : 'P') + it;
  if(!has(it, META)){
    // can't set metadata to uncaught frozen object
    if(!isExtensible(it))return 'F';
    // not necessary to add metadata
    if(!create)return 'E';
    // add missing metadata
    setMeta(it);
  // return object ID
  } return it[META].i;
};
var getWeak = function(it, create){
  if(!has(it, META)){
    // can't set metadata to uncaught frozen object
    if(!isExtensible(it))return true;
    // not necessary to add metadata
    if(!create)return false;
    // add missing metadata
    setMeta(it);
  // return hash weak collections IDs
  } return it[META].w;
};
// add metadata on freeze-family methods calling
var onFreeze = function(it){
  if(FREEZE && meta.NEED && isExtensible(it) && !has(it, META))setMeta(it);
  return it;
};
var meta = module.exports = {
  KEY:      META,
  NEED:     false,
  fastKey:  fastKey,
  getWeak:  getWeak,
  onFreeze: onFreeze
};

/***/ }),
/* 46 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.2 / 15.2.3.5 Object.create(O [, Properties])
var anObject    = __webpack_require__(10)
  , dPs         = __webpack_require__(124)
  , enumBugKeys = __webpack_require__(42)
  , IE_PROTO    = __webpack_require__(50)('IE_PROTO')
  , Empty       = function(){ /* empty */ }
  , PROTOTYPE   = 'prototype';

// Create object with fake `null` prototype: use iframe Object with cleared prototype
var createDict = function(){
  // Thrash, waste and sodomy: IE GC bug
  var iframe = __webpack_require__(41)('iframe')
    , i      = enumBugKeys.length
    , lt     = '<'
    , gt     = '>'
    , iframeDocument;
  iframe.style.display = 'none';
  __webpack_require__(67).appendChild(iframe);
  iframe.src = 'javascript:'; // eslint-disable-line no-script-url
  // createDict = iframe.contentWindow.Object;
  // html.removeChild(iframe);
  iframeDocument = iframe.contentWindow.document;
  iframeDocument.open();
  iframeDocument.write(lt + 'script' + gt + 'document.F=Object' + lt + '/script' + gt);
  iframeDocument.close();
  createDict = iframeDocument.F;
  while(i--)delete createDict[PROTOTYPE][enumBugKeys[i]];
  return createDict();
};

module.exports = Object.create || function create(O, Properties){
  var result;
  if(O !== null){
    Empty[PROTOTYPE] = anObject(O);
    result = new Empty;
    Empty[PROTOTYPE] = null;
    // add "__proto__" for Object.getPrototypeOf polyfill
    result[IE_PROTO] = O;
  } else result = createDict();
  return Properties === undefined ? result : dPs(result, Properties);
};


/***/ }),
/* 47 */
/***/ (function(module, exports) {

exports.f = Object.getOwnPropertySymbols;

/***/ }),
/* 48 */
/***/ (function(module, exports, __webpack_require__) {

// most Object methods by ES6 should accept primitives
var $export = __webpack_require__(8)
  , core    = __webpack_require__(0)
  , fails   = __webpack_require__(15);
module.exports = function(KEY, exec){
  var fn  = (core.Object || {})[KEY] || Object[KEY]
    , exp = {};
  exp[KEY] = exec(fn);
  $export($export.S + $export.F * fails(function(){ fn(1); }), 'Object', exp);
};

/***/ }),
/* 49 */
/***/ (function(module, exports, __webpack_require__) {

var hide = __webpack_require__(11);
module.exports = function(target, src, safe){
  for(var key in src){
    if(safe && target[key])target[key] = src[key];
    else hide(target, key, src[key]);
  } return target;
};

/***/ }),
/* 50 */
/***/ (function(module, exports, __webpack_require__) {

var shared = __webpack_require__(51)('keys')
  , uid    = __webpack_require__(38);
module.exports = function(key){
  return shared[key] || (shared[key] = uid(key));
};

/***/ }),
/* 51 */
/***/ (function(module, exports, __webpack_require__) {

var global = __webpack_require__(2)
  , SHARED = '__core-js_shared__'
  , store  = global[SHARED] || (global[SHARED] = {});
module.exports = function(key){
  return store[key] || (store[key] = {});
};

/***/ }),
/* 52 */
/***/ (function(module, exports) {

// 7.1.4 ToInteger
var ceil  = Math.ceil
  , floor = Math.floor;
module.exports = function(it){
  return isNaN(it = +it) ? 0 : (it > 0 ? floor : ceil)(it);
};

/***/ }),
/* 53 */
/***/ (function(module, exports, __webpack_require__) {

// 7.1.1 ToPrimitive(input [, PreferredType])
var isObject = __webpack_require__(17);
// instead of the ES6 spec version, we didn't implement @@toPrimitive case
// and the second argument - flag - preferred type is a string
module.exports = function(it, S){
  if(!isObject(it))return it;
  var fn, val;
  if(S && typeof (fn = it.toString) == 'function' && !isObject(val = fn.call(it)))return val;
  if(typeof (fn = it.valueOf) == 'function' && !isObject(val = fn.call(it)))return val;
  if(!S && typeof (fn = it.toString) == 'function' && !isObject(val = fn.call(it)))return val;
  throw TypeError("Can't convert object to primitive value");
};

/***/ }),
/* 54 */
/***/ (function(module, exports, __webpack_require__) {

var global         = __webpack_require__(2)
  , core           = __webpack_require__(0)
  , LIBRARY        = __webpack_require__(35)
  , wksExt         = __webpack_require__(55)
  , defineProperty = __webpack_require__(4).f;
module.exports = function(name){
  var $Symbol = core.Symbol || (core.Symbol = LIBRARY ? {} : global.Symbol || {});
  if(name.charAt(0) != '_' && !(name in $Symbol))defineProperty($Symbol, name, {value: wksExt.f(name)});
};

/***/ }),
/* 55 */
/***/ (function(module, exports, __webpack_require__) {

exports.f = __webpack_require__(1);

/***/ }),
/* 56 */
/***/ (function(module, exports, __webpack_require__) {

var classof   = __webpack_require__(32)
  , ITERATOR  = __webpack_require__(1)('iterator')
  , Iterators = __webpack_require__(19);
module.exports = __webpack_require__(0).getIteratorMethod = function(it){
  if(it != undefined)return it[ITERATOR]
    || it['@@iterator']
    || Iterators[classof(it)];
};

/***/ }),
/* 57 */
/***/ (function(module, exports) {



/***/ }),
/* 58 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__ = __webpack_require__(7);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Symbol__ = __webpack_require__(9);
/* unused harmony export Long */
/* unused harmony export isLong */
/* unused harmony export fromInt */
/* harmony export (immutable) */ __webpack_exports__["a"] = fromNumber;
/* unused harmony export fromBits */
/* unused harmony export fromString */
/* unused harmony export fromValue */
/* unused harmony export ZERO */
/* unused harmony export UZERO */
/* unused harmony export ONE */
/* unused harmony export UONE */
/* unused harmony export NEG_ONE */
/* unused harmony export MAX_VALUE */
/* unused harmony export MAX_UNSIGNED_VALUE */
/* unused harmony export MIN_VALUE */



var Long = function () {
    function Long(low, high, unsigned) {
        __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default()(this, Long);

        this.eq = this.equals;
        this.neq = this.notEquals;
        this.lt = this.lessThan;
        this.lte = this.lessThanOrEqual;
        this.gt = this.greaterThan;
        this.gte = this.greaterThanOrEqual;
        this.comp = this.compare;
        this.neg = this.negate;
        this.abs = this.absolute;
        this.sub = this.subtract;
        this.mul = this.multiply;
        this.div = this.divide;
        this.mod = this.modulo;
        this.shl = this.shiftLeft;
        this.shr = this.shiftRight;
        this.shru = this.shiftRightUnsigned;
        this.Equals = this.equals;
        this.CompareTo = this.compare;
        this.ToString = this.toString;
        this.low = low | 0;
        this.high = high | 0;
        this.unsigned = !!unsigned;
    }

    __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default()(Long, [{
        key: 'toInt',
        value: function toInt() {
            return this.unsigned ? this.low >>> 0 : this.low;
        }
    }, {
        key: 'toNumber',
        value: function toNumber() {
            if (this.unsigned) return (this.high >>> 0) * TWO_PWR_32_DBL + (this.low >>> 0);
            return this.high * TWO_PWR_32_DBL + (this.low >>> 0);
        }
    }, {
        key: 'toString',
        value: function toString() {
            var radix = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 10;

            radix = radix || 10;
            if (radix < 2 || 36 < radix) throw RangeError('radix');
            if (this.isZero()) return '0';
            if (this.isNegative()) {
                if (this.eq(MIN_VALUE)) {
                    var radixLong = fromNumber(radix),
                        div = this.div(radixLong),
                        rem1 = div.mul(radixLong).sub(this);
                    return div.toString(radix) + rem1.toInt().toString(radix);
                } else return '-' + this.neg().toString(radix);
            }
            var radixToPower = fromNumber(pow_dbl(radix, 6), this.unsigned),
                rem = this;
            var result = '';
            while (true) {
                var remDiv = rem.div(radixToPower),
                    intval = rem.sub(remDiv.mul(radixToPower)).toInt() >>> 0,
                    digits = intval.toString(radix);
                rem = remDiv;
                if (rem.isZero()) return digits + result;else {
                    while (digits.length < 6) {
                        digits = '0' + digits;
                    }result = '' + digits + result;
                }
            }
        }
    }, {
        key: 'getHighBits',
        value: function getHighBits() {
            return this.high;
        }
    }, {
        key: 'getHighBitsUnsigned',
        value: function getHighBitsUnsigned() {
            return this.high >>> 0;
        }
    }, {
        key: 'getLowBits',
        value: function getLowBits() {
            return this.low;
        }
    }, {
        key: 'getLowBitsUnsigned',
        value: function getLowBitsUnsigned() {
            return this.low >>> 0;
        }
    }, {
        key: 'getNumBitsAbs',
        value: function getNumBitsAbs() {
            if (this.isNegative()) return this.eq(MIN_VALUE) ? 64 : this.neg().getNumBitsAbs();
            var val = this.high != 0 ? this.high : this.low;
            for (var bit = 31; bit > 0; bit--) {
                if ((val & 1 << bit) != 0) break;
            }return this.high != 0 ? bit + 33 : bit + 1;
        }
    }, {
        key: 'isZero',
        value: function isZero() {
            return this.high === 0 && this.low === 0;
        }
    }, {
        key: 'isNegative',
        value: function isNegative() {
            return !this.unsigned && this.high < 0;
        }
    }, {
        key: 'isPositive',
        value: function isPositive() {
            return this.unsigned || this.high >= 0;
        }
    }, {
        key: 'isOdd',
        value: function isOdd() {
            return (this.low & 1) === 1;
        }
    }, {
        key: 'isEven',
        value: function isEven() {
            return (this.low & 1) === 0;
        }
    }, {
        key: 'equals',
        value: function equals(other) {
            if (!isLong(other)) other = fromValue(other);
            if (this.unsigned !== other.unsigned && this.high >>> 31 === 1 && other.high >>> 31 === 1) return false;
            return this.high === other.high && this.low === other.low;
        }
    }, {
        key: 'notEquals',
        value: function notEquals(other) {
            return !this.eq(other);
        }
    }, {
        key: 'lessThan',
        value: function lessThan(other) {
            return this.comp(other) < 0;
        }
    }, {
        key: 'lessThanOrEqual',
        value: function lessThanOrEqual(other) {
            return this.comp(other) <= 0;
        }
    }, {
        key: 'greaterThan',
        value: function greaterThan(other) {
            return this.comp(other) > 0;
        }
    }, {
        key: 'greaterThanOrEqual',
        value: function greaterThanOrEqual(other) {
            return this.comp(other) >= 0;
        }
    }, {
        key: 'compare',
        value: function compare(other) {
            if (!isLong(other)) other = fromValue(other);
            if (this.eq(other)) return 0;
            var thisNeg = this.isNegative(),
                otherNeg = other.isNegative();
            if (thisNeg && !otherNeg) return -1;
            if (!thisNeg && otherNeg) return 1;
            if (!this.unsigned) return this.sub(other).isNegative() ? -1 : 1;
            return other.high >>> 0 > this.high >>> 0 || other.high === this.high && other.low >>> 0 > this.low >>> 0 ? -1 : 1;
        }
    }, {
        key: 'negate',
        value: function negate() {
            if (!this.unsigned && this.eq(MIN_VALUE)) return MIN_VALUE;
            return this.not().add(ONE);
        }
    }, {
        key: 'absolute',
        value: function absolute() {
            if (!this.unsigned && this.isNegative()) return this.negate();else return this;
        }
    }, {
        key: 'add',
        value: function add(addend) {
            if (!isLong(addend)) addend = fromValue(addend);
            var a48 = this.high >>> 16;
            var a32 = this.high & 0xFFFF;
            var a16 = this.low >>> 16;
            var a00 = this.low & 0xFFFF;
            var b48 = addend.high >>> 16;
            var b32 = addend.high & 0xFFFF;
            var b16 = addend.low >>> 16;
            var b00 = addend.low & 0xFFFF;
            var c48 = 0,
                c32 = 0,
                c16 = 0,
                c00 = 0;
            c00 += a00 + b00;
            c16 += c00 >>> 16;
            c00 &= 0xFFFF;
            c16 += a16 + b16;
            c32 += c16 >>> 16;
            c16 &= 0xFFFF;
            c32 += a32 + b32;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c48 += a48 + b48;
            c48 &= 0xFFFF;
            return fromBits(c16 << 16 | c00, c48 << 16 | c32, this.unsigned);
        }
    }, {
        key: 'subtract',
        value: function subtract(subtrahend) {
            if (!isLong(subtrahend)) subtrahend = fromValue(subtrahend);
            return this.add(subtrahend.neg());
        }
    }, {
        key: 'multiply',
        value: function multiply(multiplier) {
            if (this.isZero()) return ZERO;
            if (!isLong(multiplier)) multiplier = fromValue(multiplier);
            if (multiplier.isZero()) return ZERO;
            if (this.eq(MIN_VALUE)) return multiplier.isOdd() ? MIN_VALUE : ZERO;
            if (multiplier.eq(MIN_VALUE)) return this.isOdd() ? MIN_VALUE : ZERO;
            if (this.isNegative()) {
                if (multiplier.isNegative()) return this.neg().mul(multiplier.neg());else return this.neg().mul(multiplier).neg();
            } else if (multiplier.isNegative()) return this.mul(multiplier.neg()).neg();
            if (this.lt(TWO_PWR_24) && multiplier.lt(TWO_PWR_24)) return fromNumber(this.toNumber() * multiplier.toNumber(), this.unsigned);
            var a48 = this.high >>> 16;
            var a32 = this.high & 0xFFFF;
            var a16 = this.low >>> 16;
            var a00 = this.low & 0xFFFF;
            var b48 = multiplier.high >>> 16;
            var b32 = multiplier.high & 0xFFFF;
            var b16 = multiplier.low >>> 16;
            var b00 = multiplier.low & 0xFFFF;
            var c48 = 0,
                c32 = 0,
                c16 = 0,
                c00 = 0;
            c00 += a00 * b00;
            c16 += c00 >>> 16;
            c00 &= 0xFFFF;
            c16 += a16 * b00;
            c32 += c16 >>> 16;
            c16 &= 0xFFFF;
            c16 += a00 * b16;
            c32 += c16 >>> 16;
            c16 &= 0xFFFF;
            c32 += a32 * b00;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c32 += a16 * b16;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c32 += a00 * b32;
            c48 += c32 >>> 16;
            c32 &= 0xFFFF;
            c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
            c48 &= 0xFFFF;
            return fromBits(c16 << 16 | c00, c48 << 16 | c32, this.unsigned);
        }
    }, {
        key: 'divide',
        value: function divide(divisor) {
            if (!isLong(divisor)) divisor = fromValue(divisor);
            if (divisor.isZero()) throw Error('division by zero');
            if (this.isZero()) return this.unsigned ? UZERO : ZERO;
            var approx = 0,
                rem = ZERO,
                res = ZERO;
            if (!this.unsigned) {
                if (this.eq(MIN_VALUE)) {
                    if (divisor.eq(ONE) || divisor.eq(NEG_ONE)) return MIN_VALUE;else if (divisor.eq(MIN_VALUE)) return ONE;else {
                        var halfThis = this.shr(1);
                        var _approx = halfThis.div(divisor).shl(1);
                        if (_approx.eq(ZERO)) {
                            return divisor.isNegative() ? ONE : NEG_ONE;
                        } else {
                            rem = this.sub(divisor.mul(_approx));
                            res = _approx.add(rem.div(divisor));
                            return res;
                        }
                    }
                } else if (divisor.eq(MIN_VALUE)) return this.unsigned ? UZERO : ZERO;
                if (this.isNegative()) {
                    if (divisor.isNegative()) return this.neg().div(divisor.neg());
                    return this.neg().div(divisor).neg();
                } else if (divisor.isNegative()) return this.div(divisor.neg()).neg();
                res = ZERO;
            } else {
                if (!divisor.unsigned) divisor = divisor.toUnsigned();
                if (divisor.gt(this)) return UZERO;
                if (divisor.gt(this.shru(1))) return UONE;
                res = UZERO;
            }
            rem = this;
            while (rem.gte(divisor)) {
                approx = Math.max(1, Math.floor(rem.toNumber() / divisor.toNumber()));
                var log2 = Math.ceil(Math.log(approx) / Math.LN2),
                    delta = log2 <= 48 ? 1 : pow_dbl(2, log2 - 48),
                    approxRes = fromNumber(approx),
                    approxRem = approxRes.mul(divisor);
                while (approxRem.isNegative() || approxRem.gt(rem)) {
                    approx -= delta;
                    approxRes = fromNumber(approx, this.unsigned);
                    approxRem = approxRes.mul(divisor);
                }
                if (approxRes.isZero()) approxRes = ONE;
                res = res.add(approxRes);
                rem = rem.sub(approxRem);
            }
            return res;
        }
    }, {
        key: 'modulo',
        value: function modulo(divisor) {
            if (!isLong(divisor)) divisor = fromValue(divisor);
            return this.sub(this.div(divisor).mul(divisor));
        }
    }, {
        key: 'not',
        value: function not() {
            return fromBits(~this.low, ~this.high, this.unsigned);
        }
    }, {
        key: 'and',
        value: function and(other) {
            if (!isLong(other)) other = fromValue(other);
            return fromBits(this.low & other.low, this.high & other.high, this.unsigned);
        }
    }, {
        key: 'or',
        value: function or(other) {
            if (!isLong(other)) other = fromValue(other);
            return fromBits(this.low | other.low, this.high | other.high, this.unsigned);
        }
    }, {
        key: 'xor',
        value: function xor(other) {
            if (!isLong(other)) other = fromValue(other);
            return fromBits(this.low ^ other.low, this.high ^ other.high, this.unsigned);
        }
    }, {
        key: 'shiftLeft',
        value: function shiftLeft(numBits) {
            if (isLong(numBits)) numBits = numBits.toInt();
            numBits = numBits & 63;
            if (numBits === 0) return this;else if (numBits < 32) return fromBits(this.low << numBits, this.high << numBits | this.low >>> 32 - numBits, this.unsigned);else return fromBits(0, this.low << numBits - 32, this.unsigned);
        }
    }, {
        key: 'shiftRight',
        value: function shiftRight(numBits) {
            if (isLong(numBits)) numBits = numBits.toInt();
            numBits = numBits & 63;
            if (numBits === 0) return this;else if (numBits < 32) return fromBits(this.low >>> numBits | this.high << 32 - numBits, this.high >> numBits, this.unsigned);else return fromBits(this.high >> numBits - 32, this.high >= 0 ? 0 : -1, this.unsigned);
        }
    }, {
        key: 'shiftRightUnsigned',
        value: function shiftRightUnsigned(numBits) {
            if (isLong(numBits)) numBits = numBits.toInt();
            numBits = numBits & 63;
            if (numBits === 0) return this;else {
                var high = this.high;
                if (numBits < 32) {
                    var low = this.low;
                    return fromBits(low >>> numBits | high << 32 - numBits, high >>> numBits, this.unsigned);
                } else if (numBits === 32) return fromBits(high, 0, this.unsigned);else return fromBits(high >>> numBits - 32, 0, this.unsigned);
            }
        }
    }, {
        key: 'toSigned',
        value: function toSigned() {
            if (!this.unsigned) return this;
            return fromBits(this.low, this.high, false);
        }
    }, {
        key: 'toUnsigned',
        value: function toUnsigned() {
            if (this.unsigned) return this;
            return fromBits(this.low, this.high, true);
        }
    }, {
        key: 'toBytes',
        value: function toBytes(le) {
            return le ? this.toBytesLE() : this.toBytesBE();
        }
    }, {
        key: 'toBytesLE',
        value: function toBytesLE() {
            var hi = this.high,
                lo = this.low;
            return [lo & 0xff, lo >>> 8 & 0xff, lo >>> 16 & 0xff, lo >>> 24 & 0xff, hi & 0xff, hi >>> 8 & 0xff, hi >>> 16 & 0xff, hi >>> 24 & 0xff];
        }
    }, {
        key: 'toBytesBE',
        value: function toBytesBE() {
            var hi = this.high,
                lo = this.low;
            return [hi >>> 24 & 0xff, hi >>> 16 & 0xff, hi >>> 8 & 0xff, hi & 0xff, lo >>> 24 & 0xff, lo >>> 16 & 0xff, lo >>> 8 & 0xff, lo & 0xff];
        }
    }, {
        key: __WEBPACK_IMPORTED_MODULE_2__Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "System.Int64",
                interfaces: ["FSharpRecord", "System.IComparable"],
                properties: {
                    low: "number",
                    high: "number",
                    unsigned: "boolean"
                }
            };
        }
    }]);

    return Long;
}();
var INT_CACHE = {};
var UINT_CACHE = {};
function isLong(obj) {
    return obj && obj instanceof Long;
}
function fromInt(value) {
    var unsigned = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

    var obj, cachedObj, cache;
    if (unsigned) {
        value >>>= 0;
        if (cache = 0 <= value && value < 256) {
            cachedObj = UINT_CACHE[value];
            if (cachedObj) return cachedObj;
        }
        obj = fromBits(value, (value | 0) < 0 ? -1 : 0, true);
        if (cache) UINT_CACHE[value] = obj;
        return obj;
    } else {
        value |= 0;
        if (cache = -128 <= value && value < 128) {
            cachedObj = INT_CACHE[value];
            if (cachedObj) return cachedObj;
        }
        obj = fromBits(value, value < 0 ? -1 : 0, false);
        if (cache) INT_CACHE[value] = obj;
        return obj;
    }
}
function fromNumber(value) {
    var unsigned = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

    if (isNaN(value) || !isFinite(value)) return unsigned ? UZERO : ZERO;
    if (unsigned) {
        if (value < 0) return UZERO;
        if (value >= TWO_PWR_64_DBL) return MAX_UNSIGNED_VALUE;
    } else {
        if (value <= -TWO_PWR_63_DBL) return MIN_VALUE;
        if (value + 1 >= TWO_PWR_63_DBL) return MAX_VALUE;
    }
    if (value < 0) return fromNumber(-value, unsigned).neg();
    return fromBits(value % TWO_PWR_32_DBL | 0, value / TWO_PWR_32_DBL | 0, unsigned);
}
function fromBits(lowBits, highBits, unsigned) {
    return new Long(lowBits, highBits, unsigned);
}
var pow_dbl = Math.pow;
function fromString(str) {
    var unsigned = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;
    var radix = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 10;

    if (str.length === 0) throw Error('empty string');
    if (str === "NaN" || str === "Infinity" || str === "+Infinity" || str === "-Infinity") return ZERO;
    if (typeof unsigned === 'number') {
        radix = unsigned, unsigned = false;
    } else {
        unsigned = !!unsigned;
    }
    radix = radix || 10;
    if (radix < 2 || 36 < radix) throw RangeError('radix');
    var p = str.indexOf('-');
    if (p > 0) throw Error('interior hyphen');else if (p === 0) {
        return fromString(str.substring(1), unsigned, radix).neg();
    }
    var radixToPower = fromNumber(pow_dbl(radix, 8));
    var result = ZERO;
    for (var i = 0; i < str.length; i += 8) {
        var size = Math.min(8, str.length - i),
            value = parseInt(str.substring(i, i + size), radix);
        if (size < 8) {
            var power = fromNumber(pow_dbl(radix, size));
            result = result.mul(power).add(fromNumber(value));
        } else {
            result = result.mul(radixToPower);
            result = result.add(fromNumber(value));
        }
    }
    result.unsigned = unsigned;
    return result;
}
function fromValue(val) {
    if (val instanceof Long) return val;
    if (typeof val === 'number') return fromNumber(val);
    if (typeof val === 'string') return fromString(val);
    return fromBits(val.low, val.high, val.unsigned);
}
var TWO_PWR_16_DBL = 1 << 16;
var TWO_PWR_24_DBL = 1 << 24;
var TWO_PWR_32_DBL = TWO_PWR_16_DBL * TWO_PWR_16_DBL;
var TWO_PWR_64_DBL = TWO_PWR_32_DBL * TWO_PWR_32_DBL;
var TWO_PWR_63_DBL = TWO_PWR_64_DBL / 2;
var TWO_PWR_24 = fromInt(TWO_PWR_24_DBL);
var ZERO = fromInt(0);
var UZERO = fromInt(0, true);
var ONE = fromInt(1);
var UONE = fromInt(1, true);
var NEG_ONE = fromInt(-1);
var MAX_VALUE = fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0, false);
var MAX_UNSIGNED_VALUE = fromBits(0xFFFFFFFF | 0, 0xFFFFFFFF | 0, true);
var MIN_VALUE = fromBits(0, 0x80000000 | 0, false);

/***/ }),
/* 59 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray__ = __webpack_require__(94);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from__ = __webpack_require__(31);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty__ = __webpack_require__(65);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator__ = __webpack_require__(13);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_createClass__ = __webpack_require__(7);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7__Util__ = __webpack_require__(3);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8__Array__ = __webpack_require__(83);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9__ListClass__ = __webpack_require__(84);
/* unused harmony export Enumerator */
/* unused harmony export getEnumerator */
/* unused harmony export toIterator */
/* unused harmony export toList */
/* unused harmony export ofList */
/* unused harmony export ofArray */
/* unused harmony export append */
/* unused harmony export average */
/* unused harmony export averageBy */
/* unused harmony export concat */
/* harmony export (immutable) */ __webpack_exports__["d"] = collect;
/* unused harmony export choose */
/* unused harmony export compareWith */
/* harmony export (immutable) */ __webpack_exports__["b"] = delay;
/* unused harmony export empty */
/* unused harmony export enumerateWhile */
/* unused harmony export enumerateThenFinally */
/* unused harmony export enumerateUsing */
/* unused harmony export exactlyOne */
/* unused harmony export except */
/* harmony export (immutable) */ __webpack_exports__["k"] = exists;
/* unused harmony export exists2 */
/* harmony export (immutable) */ __webpack_exports__["i"] = filter;
/* unused harmony export where */
/* harmony export (immutable) */ __webpack_exports__["j"] = fold;
/* unused harmony export foldBack */
/* unused harmony export fold2 */
/* unused harmony export foldBack2 */
/* harmony export (immutable) */ __webpack_exports__["f"] = forAll;
/* unused harmony export forAll2 */
/* unused harmony export tryHead */
/* unused harmony export head */
/* unused harmony export initialize */
/* unused harmony export initializeInfinite */
/* unused harmony export tryItem */
/* harmony export (immutable) */ __webpack_exports__["e"] = item;
/* unused harmony export iterate */
/* unused harmony export iterate2 */
/* unused harmony export iterateIndexed */
/* unused harmony export iterateIndexed2 */
/* unused harmony export isEmpty */
/* unused harmony export tryLast */
/* unused harmony export last */
/* unused harmony export count */
/* harmony export (immutable) */ __webpack_exports__["a"] = map;
/* unused harmony export mapIndexed */
/* unused harmony export map2 */
/* unused harmony export mapIndexed2 */
/* unused harmony export map3 */
/* unused harmony export mapFold */
/* unused harmony export mapFoldBack */
/* unused harmony export max */
/* unused harmony export maxBy */
/* unused harmony export min */
/* unused harmony export minBy */
/* unused harmony export pairwise */
/* unused harmony export permute */
/* harmony export (immutable) */ __webpack_exports__["h"] = rangeStep;
/* unused harmony export rangeChar */
/* harmony export (immutable) */ __webpack_exports__["c"] = range;
/* unused harmony export readOnly */
/* unused harmony export reduce */
/* unused harmony export reduceBack */
/* unused harmony export replicate */
/* unused harmony export reverse */
/* unused harmony export scan */
/* unused harmony export scanBack */
/* harmony export (immutable) */ __webpack_exports__["g"] = singleton;
/* unused harmony export skip */
/* unused harmony export skipWhile */
/* unused harmony export sortWith */
/* unused harmony export sum */
/* unused harmony export sumBy */
/* unused harmony export tail */
/* unused harmony export take */
/* unused harmony export truncate */
/* unused harmony export takeWhile */
/* unused harmony export tryFind */
/* unused harmony export find */
/* unused harmony export tryFindBack */
/* unused harmony export findBack */
/* unused harmony export tryFindIndex */
/* unused harmony export findIndex */
/* unused harmony export tryFindIndexBack */
/* unused harmony export findIndexBack */
/* unused harmony export tryPick */
/* unused harmony export pick */
/* unused harmony export unfold */
/* unused harmony export zip */
/* unused harmony export zip3 */











var Enumerator = function () {
    function Enumerator(iter) {
        __WEBPACK_IMPORTED_MODULE_5_babel_runtime_helpers_classCallCheck___default()(this, Enumerator);

        this.iter = iter;
    }

    __WEBPACK_IMPORTED_MODULE_6_babel_runtime_helpers_createClass___default()(Enumerator, [{
        key: "MoveNext",
        value: function MoveNext() {
            var cur = this.iter.next();
            this.current = cur.value;
            return !cur.done;
        }
    }, {
        key: "Reset",
        value: function Reset() {
            throw new Error("JS iterators cannot be reset");
        }
    }, {
        key: "Dispose",
        value: function Dispose() {}
    }, {
        key: "Current",
        get: function get() {
            return this.current;
        }
    }]);

    return Enumerator;
}();
function getEnumerator(o) {
    return typeof o.GetEnumerator === "function" ? o.GetEnumerator() : new Enumerator(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(o));
}
function toIterator(en) {
    return {
        next: function next() {
            return en.MoveNext() ? { done: false, value: en.Current } : { done: true, value: null };
        }
    };
}
function __failIfNone(res) {
    if (res == null) throw new Error("Seq did not contain any matching element");
    return res;
}
function toList(xs) {
    return foldBack(function (x, acc) {
        return new __WEBPACK_IMPORTED_MODULE_9__ListClass__["a" /* default */](x, acc);
    }, xs, new __WEBPACK_IMPORTED_MODULE_9__ListClass__["a" /* default */]());
}
function ofList(xs) {
    return delay(function () {
        return unfold(function (x) {
            return x.tail != null ? [x.head, x.tail] : null;
        }, xs);
    });
}
function ofArray(xs) {
    return delay(function () {
        return unfold(function (i) {
            return i < xs.length ? [xs[i], i + 1] : null;
        }, 0);
    });
}
function append(xs, ys) {
    return delay(function () {
        var firstDone = false;
        var i = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var iters = [i, null];
        return unfold(function () {
            var cur = void 0;
            if (!firstDone) {
                cur = iters[0].next();
                if (!cur.done) {
                    return [cur.value, iters];
                } else {
                    firstDone = true;
                    iters = [null, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys)];
                }
            }
            cur = iters[1].next();
            return !cur.done ? [cur.value, iters] : null;
        }, iters);
    });
}
function average(xs) {
    var count = 1;
    var sum = reduce(function (acc, x) {
        count++;
        return acc + x;
    }, xs);
    return sum / count;
}
function averageBy(f, xs) {
    var count = 1;
    var sum = reduce(function (acc, x) {
        count++;
        return (count === 2 ? f(acc) : acc) + f(x);
    }, xs);
    return sum / count;
}
function concat(xs) {
    return delay(function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var output = null;
        return unfold(function (innerIter) {
            var hasFinished = false;
            while (!hasFinished) {
                if (innerIter == null) {
                    var cur = iter.next();
                    if (!cur.done) {
                        innerIter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(cur.value);
                    } else {
                        hasFinished = true;
                    }
                } else {
                    var _cur = innerIter.next();
                    if (!_cur.done) {
                        output = _cur.value;
                        hasFinished = true;
                    } else {
                        innerIter = null;
                    }
                }
            }
            return innerIter != null && output != null ? [output, innerIter] : null;
        }, null);
    });
}
function collect(f, xs) {
    return concat(map(f, xs));
}
function choose(f, xs) {
    var trySkipToNext = function trySkipToNext(iter) {
        var cur = iter.next();
        if (!cur.done) {
            var y = f(cur.value);
            return y != null ? [y, iter] : trySkipToNext(iter);
        }
        return void 0;
    };
    return delay(function () {
        return unfold(function (iter) {
            return trySkipToNext(iter);
        }, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
    });
}
function compareWith(f, xs, ys) {
    var nonZero = tryFind(function (i) {
        return i != 0;
    }, map2(function (x, y) {
        return f(x, y);
    }, xs, ys));
    return nonZero != null ? nonZero : count(xs) - count(ys);
}
function delay(f) {
    return __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default()({}, __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default.a, function () {
        return __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(f());
    });
}
function empty() {
    return unfold(function () {
        return void 0;
    });
}
function enumerateWhile(cond, xs) {
    return concat(unfold(function () {
        return cond() ? [xs, true] : null;
    }));
}
function enumerateThenFinally(xs, finalFn) {
    return delay(function () {
        var iter = void 0;
        try {
            iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        } catch (err) {
            return void 0;
        } finally {
            finalFn();
        }
        return unfold(function (iter) {
            try {
                var cur = iter.next();
                return !cur.done ? [cur.value, iter] : null;
            } catch (err) {
                return void 0;
            } finally {
                finalFn();
            }
        }, iter);
    });
}
function enumerateUsing(disp, work) {
    var isDisposed = false;
    var disposeOnce = function disposeOnce() {
        if (!isDisposed) {
            isDisposed = true;
            disp.Dispose();
        }
    };
    try {
        return enumerateThenFinally(work(disp), disposeOnce);
    } catch (err) {
        return void 0;
    } finally {
        disposeOnce();
    }
}
function exactlyOne(xs) {
    var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var fst = iter.next();
    if (fst.done) throw new Error("Seq was empty");
    var snd = iter.next();
    if (!snd.done) throw new Error("Seq had multiple items");
    return fst.value;
}
function except(itemsToExclude, source) {
    var exclusionItems = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(itemsToExclude);
    var testIsNotInExclusionItems = function testIsNotInExclusionItems(element) {
        return !exclusionItems.some(function (excludedItem) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Util__["a" /* equals */])(excludedItem, element);
        });
    };
    return filter(testIsNotInExclusionItems, source);
}
function exists(f, xs) {
    function aux(iter) {
        var cur = iter.next();
        return !cur.done && (f(cur.value) || aux(iter));
    }
    return aux(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
}
function exists2(f, xs, ys) {
    function aux(iter1, iter2) {
        var cur1 = iter1.next(),
            cur2 = iter2.next();
        return !cur1.done && !cur2.done && (f(cur1.value, cur2.value) || aux(iter1, iter2));
    }
    return aux(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs), __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys));
}
function filter(f, xs) {
    function trySkipToNext(iter) {
        var cur = iter.next();
        while (!cur.done) {
            if (f(cur.value)) {
                return [cur.value, iter];
            }
            cur = iter.next();
        }
        return void 0;
    }
    return delay(function () {
        return unfold(trySkipToNext, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
    });
}
function where(f, xs) {
    return filter(f, xs);
}
function fold(f, acc, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) {
        return xs.reduce(f, acc);
    } else {
        var cur = void 0;
        for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
            cur = iter.next();
            if (cur.done) break;
            acc = f(acc, cur.value, i);
        }
        return acc;
    }
}
function foldBack(f, xs, acc) {
    var arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    for (var i = arr.length - 1; i >= 0; i--) {
        acc = f(arr[i], acc, i);
    }
    return acc;
}
function fold2(f, acc, xs, ys) {
    var iter1 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs),
        iter2 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys);
    var cur1 = void 0,
        cur2 = void 0;
    for (var i = 0;; i++) {
        cur1 = iter1.next();
        cur2 = iter2.next();
        if (cur1.done || cur2.done) {
            break;
        }
        acc = f(acc, cur1.value, cur2.value, i);
    }
    return acc;
}
function foldBack2(f, xs, ys, acc) {
    var ar1 = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    var ar2 = Array.isArray(ys) || ArrayBuffer.isView(ys) ? ys : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(ys);
    for (var i = ar1.length - 1; i >= 0; i--) {
        acc = f(ar1[i], ar2[i], acc, i);
    }
    return acc;
}
function forAll(f, xs) {
    return fold(function (acc, x) {
        return acc && f(x);
    }, true, xs);
}
function forAll2(f, xs, ys) {
    return fold2(function (acc, x, y) {
        return acc && f(x, y);
    }, true, xs, ys);
}
function tryHead(xs) {
    var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var cur = iter.next();
    return cur.done ? null : cur.value;
}
function head(xs) {
    return __failIfNone(tryHead(xs));
}
function initialize(n, f) {
    return delay(function () {
        return unfold(function (i) {
            return i < n ? [f(i), i + 1] : null;
        }, 0);
    });
}
function initializeInfinite(f) {
    return delay(function () {
        return unfold(function (i) {
            return [f(i), i + 1];
        }, 0);
    });
}
function tryItem(i, xs) {
    if (i < 0) return null;
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) return i < xs.length ? xs[i] : null;
    for (var j = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; j++) {
        var cur = iter.next();
        if (cur.done) return null;
        if (j === i) return cur.value;
    }
}
function item(i, xs) {
    return __failIfNone(tryItem(i, xs));
}
function iterate(f, xs) {
    fold(function (_, x) {
        return f(x);
    }, null, xs);
}
function iterate2(f, xs, ys) {
    fold2(function (_, x, y) {
        return f(x, y);
    }, null, xs, ys);
}
function iterateIndexed(f, xs) {
    fold(function (_, x, i) {
        return f(i, x);
    }, null, xs);
}
function iterateIndexed2(f, xs, ys) {
    fold2(function (_, x, y, i) {
        return f(i, x, y);
    }, null, xs, ys);
}
function isEmpty(xs) {
    var i = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    return i.next().done;
}
function tryLast(xs) {
    try {
        return reduce(function (_, x) {
            return x;
        }, xs);
    } catch (err) {
        return null;
    }
}
function last(xs) {
    return __failIfNone(tryLast(xs));
}
function count(xs) {
    return Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.length : fold(function (acc, x) {
        return acc + 1;
    }, 0, xs);
}
function map(f, xs) {
    return delay(function () {
        return unfold(function (iter) {
            var cur = iter.next();
            return !cur.done ? [f(cur.value), iter] : null;
        }, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
    });
}
function mapIndexed(f, xs) {
    return delay(function () {
        var i = 0;
        return unfold(function (iter) {
            var cur = iter.next();
            return !cur.done ? [f(i++, cur.value), iter] : null;
        }, __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs));
    });
}
function map2(f, xs, ys) {
    return delay(function () {
        var iter1 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var iter2 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys);
        return unfold(function () {
            var cur1 = iter1.next(),
                cur2 = iter2.next();
            return !cur1.done && !cur2.done ? [f(cur1.value, cur2.value), null] : null;
        });
    });
}
function mapIndexed2(f, xs, ys) {
    return delay(function () {
        var i = 0;
        var iter1 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var iter2 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys);
        return unfold(function () {
            var cur1 = iter1.next(),
                cur2 = iter2.next();
            return !cur1.done && !cur2.done ? [f(i++, cur1.value, cur2.value), null] : null;
        });
    });
}
function map3(f, xs, ys, zs) {
    return delay(function () {
        var iter1 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        var iter2 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(ys);
        var iter3 = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(zs);
        return unfold(function () {
            var cur1 = iter1.next(),
                cur2 = iter2.next(),
                cur3 = iter3.next();
            return !cur1.done && !cur2.done && !cur3.done ? [f(cur1.value, cur2.value, cur3.value), null] : null;
        });
    });
}
function mapFold(f, acc, xs) {
    var result = [];
    var r = void 0;
    var cur = void 0;
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        cur = iter.next();
        if (cur.done) break;

        var _f = f(acc, cur.value);

        var _f2 = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray___default()(_f, 2);

        r = _f2[0];
        acc = _f2[1];

        result.push(r);
    }
    return [result, acc];
}
function mapFoldBack(f, xs, acc) {
    var arr = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    var result = [];
    var r = void 0;
    for (var i = arr.length - 1; i >= 0; i--) {
        var _f3 = f(arr[i], acc);

        var _f4 = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_slicedToArray___default()(_f3, 2);

        r = _f4[0];
        acc = _f4[1];

        result.push(r);
    }
    return [result, acc];
}
function max(xs) {
    return reduce(function (acc, x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Util__["d" /* compare */])(acc, x) === 1 ? acc : x;
    }, xs);
}
function maxBy(f, xs) {
    return reduce(function (acc, x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Util__["d" /* compare */])(f(acc), f(x)) === 1 ? acc : x;
    }, xs);
}
function min(xs) {
    return reduce(function (acc, x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Util__["d" /* compare */])(acc, x) === -1 ? acc : x;
    }, xs);
}
function minBy(f, xs) {
    return reduce(function (acc, x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__Util__["d" /* compare */])(f(acc), f(x)) === -1 ? acc : x;
    }, xs);
}
function pairwise(xs) {
    return skip(2, scan(function (last, next) {
        return [last[1], next];
    }, [0, 0], xs));
}
function permute(f, xs) {
    return ofArray(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Array__["a" /* permute */])(f, __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs)));
}
function rangeStep(first, step, last) {
    if (step === 0) throw new Error("Step cannot be 0");
    return delay(function () {
        return unfold(function (x) {
            return step > 0 && x <= last || step < 0 && x >= last ? [x, x + step] : null;
        }, first);
    });
}
function rangeChar(first, last) {
    return delay(function () {
        return unfold(function (x) {
            return x <= last ? [x, String.fromCharCode(x.charCodeAt(0) + 1)] : null;
        }, first);
    });
}
function range(first, last) {
    return rangeStep(first, 1, last);
}
function readOnly(xs) {
    return map(function (x) {
        return x;
    }, xs);
}
function reduce(f, xs) {
    if (Array.isArray(xs) || ArrayBuffer.isView(xs)) return xs.reduce(f);
    var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var cur = iter.next();
    if (cur.done) throw new Error("Seq was empty");
    var acc = cur.value;
    for (;;) {
        cur = iter.next();
        if (cur.done) break;
        acc = f(acc, cur.value);
    }
    return acc;
}
function reduceBack(f, xs) {
    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    if (ar.length === 0) throw new Error("Seq was empty");
    var acc = ar[ar.length - 1];
    for (var i = ar.length - 2; i >= 0; i--) {
        acc = f(ar[i], acc, i);
    }return acc;
}
function replicate(n, x) {
    return initialize(n, function () {
        return x;
    });
}
function reverse(xs) {
    var ar = Array.isArray(xs) || ArrayBuffer.isView(xs) ? xs.slice(0) : __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    return ofArray(ar.reverse());
}
function scan(f, seed, xs) {
    return delay(function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        return unfold(function (acc) {
            if (acc == null) return [seed, seed];
            var cur = iter.next();
            if (!cur.done) {
                acc = f(acc, cur.value);
                return [acc, acc];
            }
            return void 0;
        }, null);
    });
}
function scanBack(f, xs, seed) {
    return reverse(scan(function (acc, x) {
        return f(x, acc);
    }, seed, reverse(xs)));
}
function singleton(x) {
    return unfold(function (x) {
        return x != null ? [x, null] : null;
    }, x);
}
function skip(n, xs) {
    return __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default()({}, __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default.a, function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        for (var i = 1; i <= n; i++) {
            if (iter.next().done) throw new Error("Seq has not enough elements");
        }return iter;
    });
}
function skipWhile(f, xs) {
    return delay(function () {
        var hasPassed = false;
        return filter(function (x) {
            return hasPassed || (hasPassed = !f(x));
        }, xs);
    });
}
function sortWith(f, xs) {
    var ys = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    return ofArray(ys.sort(f));
}
function sum(xs) {
    return fold(function (acc, x) {
        return acc + x;
    }, 0, xs);
}
function sumBy(f, xs) {
    return fold(function (acc, x) {
        return acc + f(x);
    }, 0, xs);
}
function tail(xs) {
    var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
    var cur = iter.next();
    if (cur.done) throw new Error("Seq was empty");
    return __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default()({}, __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default.a, function () {
        return iter;
    });
}
function take(n, xs) {
    var truncate = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

    return delay(function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        return unfold(function (i) {
            if (i < n) {
                var cur = iter.next();
                if (!cur.done) return [cur.value, i + 1];
                if (!truncate) throw new Error("Seq has not enough elements");
            }
            return void 0;
        }, 0);
    });
}
function truncate(n, xs) {
    return take(n, xs, true);
}
function takeWhile(f, xs) {
    return delay(function () {
        var iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);
        return unfold(function (i) {
            var cur = iter.next();
            if (!cur.done && f(cur.value)) return [cur.value, null];
            return void 0;
        }, 0);
    });
}
function tryFind(f, xs, defaultValue) {
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) return defaultValue === void 0 ? null : defaultValue;
        if (f(cur.value, i)) return cur.value;
    }
}
function find(f, xs) {
    return __failIfNone(tryFind(f, xs));
}
function tryFindBack(f, xs, defaultValue) {
    var match = null;
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) return match === null ? defaultValue === void 0 ? null : defaultValue : match;
        if (f(cur.value, i)) match = cur.value;
    }
}
function findBack(f, xs) {
    return __failIfNone(tryFindBack(f, xs));
}
function tryFindIndex(f, xs) {
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) return null;
        if (f(cur.value, i)) return i;
    }
}
function findIndex(f, xs) {
    return __failIfNone(tryFindIndex(f, xs));
}
function tryFindIndexBack(f, xs) {
    var match = -1;
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) return match === -1 ? null : match;
        if (f(cur.value, i)) match = i;
    }
}
function findIndexBack(f, xs) {
    return __failIfNone(tryFindIndexBack(f, xs));
}
function tryPick(f, xs) {
    for (var i = 0, iter = __WEBPACK_IMPORTED_MODULE_4_babel_runtime_core_js_get_iterator___default()(xs);; i++) {
        var cur = iter.next();
        if (cur.done) break;
        var y = f(cur.value, i);
        if (y != null) return y;
    }
    return void 0;
}
function pick(f, xs) {
    return __failIfNone(tryPick(f, xs));
}
function unfold(f, acc) {
    return __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_defineProperty___default()({}, __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_symbol_iterator___default.a, function () {
        return {
            next: function next() {
                var res = f(acc);
                if (res != null) {
                    acc = res[1];
                    return { done: false, value: res[0] };
                }
                return { done: true };
            }
        };
    });
}
function zip(xs, ys) {
    return map2(function (x, y) {
        return [x, y];
    }, xs, ys);
}
function zip3(xs, ys, zs) {
    return map3(function (x, y, z) {
        return [x, y, z];
    }, xs, ys, zs);
}

/***/ }),
/* 60 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(98), __esModule: true };

/***/ }),
/* 61 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(99), __esModule: true };

/***/ }),
/* 62 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(101), __esModule: true };

/***/ }),
/* 63 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(103), __esModule: true };

/***/ }),
/* 64 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(106), __esModule: true };

/***/ }),
/* 65 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _defineProperty = __webpack_require__(62);

var _defineProperty2 = _interopRequireDefault(_defineProperty);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function (obj, key, value) {
  if (key in obj) {
    (0, _defineProperty2.default)(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }

  return obj;
};

/***/ }),
/* 66 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _iterator = __webpack_require__(22);

var _iterator2 = _interopRequireDefault(_iterator);

var _symbol = __webpack_require__(64);

var _symbol2 = _interopRequireDefault(_symbol);

var _typeof = typeof _symbol2.default === "function" && typeof _iterator2.default === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof _symbol2.default === "function" && obj.constructor === _symbol2.default && obj !== _symbol2.default.prototype ? "symbol" : typeof obj; };

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = typeof _symbol2.default === "function" && _typeof(_iterator2.default) === "symbol" ? function (obj) {
  return typeof obj === "undefined" ? "undefined" : _typeof(obj);
} : function (obj) {
  return obj && typeof _symbol2.default === "function" && obj.constructor === _symbol2.default && obj !== _symbol2.default.prototype ? "symbol" : typeof obj === "undefined" ? "undefined" : _typeof(obj);
};

/***/ }),
/* 67 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = __webpack_require__(2).document && document.documentElement;

/***/ }),
/* 68 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = !__webpack_require__(6) && !__webpack_require__(15)(function(){
  return Object.defineProperty(__webpack_require__(41)('div'), 'a', {get: function(){ return 7; }}).a != 7;
});

/***/ }),
/* 69 */
/***/ (function(module, exports, __webpack_require__) {

// check on default Array iterator
var Iterators  = __webpack_require__(19)
  , ITERATOR   = __webpack_require__(1)('iterator')
  , ArrayProto = Array.prototype;

module.exports = function(it){
  return it !== undefined && (Iterators.Array === it || ArrayProto[ITERATOR] === it);
};

/***/ }),
/* 70 */
/***/ (function(module, exports, __webpack_require__) {

// 7.2.2 IsArray(argument)
var cof = __webpack_require__(23);
module.exports = Array.isArray || function isArray(arg){
  return cof(arg) == 'Array';
};

/***/ }),
/* 71 */
/***/ (function(module, exports, __webpack_require__) {

// call something on iterator step with safe closing on error
var anObject = __webpack_require__(10);
module.exports = function(iterator, fn, value, entries){
  try {
    return entries ? fn(anObject(value)[0], value[1]) : fn(value);
  // 7.4.6 IteratorClose(iterator, completion)
  } catch(e){
    var ret = iterator['return'];
    if(ret !== undefined)anObject(ret.call(iterator));
    throw e;
  }
};

/***/ }),
/* 72 */
/***/ (function(module, exports, __webpack_require__) {

var ITERATOR     = __webpack_require__(1)('iterator')
  , SAFE_CLOSING = false;

try {
  var riter = [7][ITERATOR]();
  riter['return'] = function(){ SAFE_CLOSING = true; };
  Array.from(riter, function(){ throw 2; });
} catch(e){ /* empty */ }

module.exports = function(exec, skipClosing){
  if(!skipClosing && !SAFE_CLOSING)return false;
  var safe = false;
  try {
    var arr  = [7]
      , iter = arr[ITERATOR]();
    iter.next = function(){ return {done: safe = true}; };
    arr[ITERATOR] = function(){ return iter; };
    exec(arr);
  } catch(e){ /* empty */ }
  return safe;
};

/***/ }),
/* 73 */
/***/ (function(module, exports) {

module.exports = function(done, value){
  return {value: value, done: !!done};
};

/***/ }),
/* 74 */
/***/ (function(module, exports, __webpack_require__) {

var pIE            = __webpack_require__(36)
  , createDesc     = __webpack_require__(25)
  , toIObject      = __webpack_require__(12)
  , toPrimitive    = __webpack_require__(53)
  , has            = __webpack_require__(16)
  , IE8_DOM_DEFINE = __webpack_require__(68)
  , gOPD           = Object.getOwnPropertyDescriptor;

exports.f = __webpack_require__(6) ? gOPD : function getOwnPropertyDescriptor(O, P){
  O = toIObject(O);
  P = toPrimitive(P, true);
  if(IE8_DOM_DEFINE)try {
    return gOPD(O, P);
  } catch(e){ /* empty */ }
  if(has(O, P))return createDesc(!pIE.f.call(O, P), O[P]);
};

/***/ }),
/* 75 */
/***/ (function(module, exports, __webpack_require__) {

// fallback for IE11 buggy Object.getOwnPropertyNames with iframe and window
var toIObject = __webpack_require__(12)
  , gOPN      = __webpack_require__(76).f
  , toString  = {}.toString;

var windowNames = typeof window == 'object' && window && Object.getOwnPropertyNames
  ? Object.getOwnPropertyNames(window) : [];

var getWindowNames = function(it){
  try {
    return gOPN(it);
  } catch(e){
    return windowNames.slice();
  }
};

module.exports.f = function getOwnPropertyNames(it){
  return windowNames && toString.call(it) == '[object Window]' ? getWindowNames(it) : gOPN(toIObject(it));
};


/***/ }),
/* 76 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.7 / 15.2.3.4 Object.getOwnPropertyNames(O)
var $keys      = __webpack_require__(78)
  , hiddenKeys = __webpack_require__(42).concat('length', 'prototype');

exports.f = Object.getOwnPropertyNames || function getOwnPropertyNames(O){
  return $keys(O, hiddenKeys);
};

/***/ }),
/* 77 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.9 / 15.2.3.2 Object.getPrototypeOf(O)
var has         = __webpack_require__(16)
  , toObject    = __webpack_require__(27)
  , IE_PROTO    = __webpack_require__(50)('IE_PROTO')
  , ObjectProto = Object.prototype;

module.exports = Object.getPrototypeOf || function(O){
  O = toObject(O);
  if(has(O, IE_PROTO))return O[IE_PROTO];
  if(typeof O.constructor == 'function' && O instanceof O.constructor){
    return O.constructor.prototype;
  } return O instanceof Object ? ObjectProto : null;
};

/***/ }),
/* 78 */
/***/ (function(module, exports, __webpack_require__) {

var has          = __webpack_require__(16)
  , toIObject    = __webpack_require__(12)
  , arrayIndexOf = __webpack_require__(110)(false)
  , IE_PROTO     = __webpack_require__(50)('IE_PROTO');

module.exports = function(object, names){
  var O      = toIObject(object)
    , i      = 0
    , result = []
    , key;
  for(key in O)if(key != IE_PROTO)has(O, key) && result.push(key);
  // Don't enum bug & hidden keys
  while(names.length > i)if(has(O, key = names[i++])){
    ~arrayIndexOf(result, key) || result.push(key);
  }
  return result;
};

/***/ }),
/* 79 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = __webpack_require__(11);

/***/ }),
/* 80 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var global      = __webpack_require__(2)
  , core        = __webpack_require__(0)
  , dP          = __webpack_require__(4)
  , DESCRIPTORS = __webpack_require__(6)
  , SPECIES     = __webpack_require__(1)('species');

module.exports = function(KEY){
  var C = typeof core[KEY] == 'function' ? core[KEY] : global[KEY];
  if(DESCRIPTORS && C && !C[SPECIES])dP.f(C, SPECIES, {
    configurable: true,
    get: function(){ return this; }
  });
};

/***/ }),
/* 81 */
/***/ (function(module, exports, __webpack_require__) {

var ctx                = __webpack_require__(14)
  , invoke             = __webpack_require__(119)
  , html               = __webpack_require__(67)
  , cel                = __webpack_require__(41)
  , global             = __webpack_require__(2)
  , process            = global.process
  , setTask            = global.setImmediate
  , clearTask          = global.clearImmediate
  , MessageChannel     = global.MessageChannel
  , counter            = 0
  , queue              = {}
  , ONREADYSTATECHANGE = 'onreadystatechange'
  , defer, channel, port;
var run = function(){
  var id = +this;
  if(queue.hasOwnProperty(id)){
    var fn = queue[id];
    delete queue[id];
    fn();
  }
};
var listener = function(event){
  run.call(event.data);
};
// Node.js 0.9+ & IE10+ has setImmediate, otherwise:
if(!setTask || !clearTask){
  setTask = function setImmediate(fn){
    var args = [], i = 1;
    while(arguments.length > i)args.push(arguments[i++]);
    queue[++counter] = function(){
      invoke(typeof fn == 'function' ? fn : Function(fn), args);
    };
    defer(counter);
    return counter;
  };
  clearTask = function clearImmediate(id){
    delete queue[id];
  };
  // Node.js 0.8-
  if(__webpack_require__(23)(process) == 'process'){
    defer = function(id){
      process.nextTick(ctx(run, id, 1));
    };
  // Browsers with MessageChannel, includes WebWorkers
  } else if(MessageChannel){
    channel = new MessageChannel;
    port    = channel.port2;
    channel.port1.onmessage = listener;
    defer = ctx(port.postMessage, port, 1);
  // Browsers with postMessage, skip WebWorkers
  // IE8 has postMessage, but it's sync & typeof its postMessage is 'object'
  } else if(global.addEventListener && typeof postMessage == 'function' && !global.importScripts){
    defer = function(id){
      global.postMessage(id + '', '*');
    };
    global.addEventListener('message', listener, false);
  // IE8-
  } else if(ONREADYSTATECHANGE in cel('script')){
    defer = function(id){
      html.appendChild(cel('script'))[ONREADYSTATECHANGE] = function(){
        html.removeChild(this);
        run.call(id);
      };
    };
  // Rest old browsers
  } else {
    defer = function(id){
      setTimeout(ctx(run, id, 1), 0);
    };
  }
}
module.exports = {
  set:   setTask,
  clear: clearTask
};

/***/ }),
/* 82 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__ = __webpack_require__(7);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Logic_fs__ = __webpack_require__(144);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__ = __webpack_require__(3);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_MailboxProcessor__ = __webpack_require__(85);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__node_modules_fable_core_AsyncBuilder__ = __webpack_require__(21);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7__node_modules_fable_core_Choice__ = __webpack_require__(30);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8__node_modules_fable_core_String__ = __webpack_require__(87);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_9__node_modules_fable_core_Async__ = __webpack_require__(29);
/* harmony export (immutable) */ __webpack_exports__["a"] = state2;
/* harmony export (immutable) */ __webpack_exports__["b"] = state;
/* harmony export (immutable) */ __webpack_exports__["c"] = n;
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "d", function() { return a; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "e", function() { return b; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "f", function() { return colors; });
/* harmony export (immutable) */ __webpack_exports__["g"] = draw;
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "h", function() { return AgentEvent; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "i", function() { return game; });
/* harmony export (immutable) */ __webpack_exports__["j"] = touchEvents;
/* harmony export (immutable) */ __webpack_exports__["k"] = setUpTouchEvents;
/* harmony export (immutable) */ __webpack_exports__["l"] = init;











function state2(x, p) {
    return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].State(__WEBPACK_IMPORTED_MODULE_2__Logic_fs__["b" /* Array2D */].ofList(x), p);
}
function state(x) {
    return state2(x, 0);
}
function n() {
    return __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].empty();
}
var a = [true, 1];
var b = [false, 1];
var colors = ["rgb(200,0,0)", "rgb(0,200,0)", "rgb(200,200,0)", "rgb(200,200,200)", "rgb(0, 0, 200)"];
function draw(state_1) {
    var span = document.getElementsByTagName('span')[0];
    span.innerText = String(state_1.Points);
    var canvas = document.getElementsByTagName('canvas')[0];
    canvas.width = 400;
    canvas.height = 600;
    var ctx = canvas.getContext('2d');
    ctx.fillStyle = "black";
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    var rectH = ~~(~~canvas.height / 15) | 0;
    var rectW = ~~(~~canvas.width / 10) | 0;

    for (var i = 0; i <= state_1.Blocks.Length1 - 1; i++) {
        for (var j = 0; j <= state_1.Blocks.Length2 - 1; j++) {
            var patternInput = state_1.Blocks.get_Item(i, j);

            if (patternInput[1] != null) {
                ctx.fillStyle = colors[patternInput[1] % colors.length];
                ctx.fillRect(i * rectW, j * rectH, rectW - ~~(rectW / 10), rectH - ~~(rectH / 10));
            }
        }
    }
}
var AgentEvent = function () {
    function AgentEvent(tag, data) {
        __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_classCallCheck___default()(this, AgentEvent);

        this.tag = tag;
        this.data = data;
    }

    __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_createClass___default()(AgentEvent, [{
        key: __WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "fable.AgentEvent",
                interfaces: ["FSharpUnion", "System.IEquatable", "System.IComparable"],
                cases: [["Time"], ["KeyPressed", __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed]]
            };
        }
    }, {
        key: "Equals",
        value: function Equals(other) {
            return this === other || this.tag === other.tag && __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["a" /* equals */])(this.data, other.data);
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(other) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["b" /* compareUnions */])(this, other) | 0;
        }
    }]);

    return AgentEvent;
}();
__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["b" /* setType */])("fable.AgentEvent", AgentEvent);
var game = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_MailboxProcessor__["a" /* start */])(function (inbox) {
    var rnd = {};

    var loop = function loop(state_1) {
        return function (builder_) {
            return builder_.Delay(function () {
                return builder_.TryWith(builder_.Delay(function () {
                    return builder_.Bind(inbox.receive(), function (_arg1) {
                        var state_4 = _arg1.tag === 1 ? __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].score(function (state_2) {
                            return __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].applyKeyboardTransition(_arg1.data, state_2);
                        }(state_1)) : __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].score(function (state_3) {
                            return __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].applyTransition(rnd, state_3);
                        }(__WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].move(state_1)));
                        draw(state_4);
                        return builder_.ReturnFrom(loop(state_4));
                    });
                }), function (_arg2) {
                    console.error(_arg2);
                    return builder_.ReturnFrom(loop(state_1));
                });
            });
        }(__WEBPACK_IMPORTED_MODULE_6__node_modules_fable_core_AsyncBuilder__["a" /* singleton */]);
    };

    return loop(__WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].initState);
});
window.addEventListener('keypress', function (x) {
    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["c" /* defaultArg */])(function () {
        var matchValue = String.fromCharCode(~~x.charCode);
        var $var3 = matchValue === "A" ? [1] : matchValue === "D" ? [2] : matchValue === "S" ? [3] : matchValue === "W" ? [0] : matchValue === "a" ? [1] : matchValue === "d" ? [2] : matchValue === "s" ? [3] : matchValue === "w" ? [0] : [4];

        switch ($var3[0]) {
            case 0:
                return function () {
                    var inputRecord = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                    return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(true, inputRecord.Down, inputRecord.Left, inputRecord.Right);
                }();

            case 1:
                return function () {
                    var inputRecord_1 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                    return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(inputRecord_1.Up, inputRecord_1.Down, true, inputRecord_1.Right);
                }();

            case 2:
                return function () {
                    var inputRecord_2 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                    return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(inputRecord_2.Up, inputRecord_2.Down, inputRecord_2.Left, true);
                }();

            case 3:
                return function () {
                    var inputRecord_3 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                    return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(inputRecord_3.Up, true, inputRecord_3.Left, inputRecord_3.Right);
                }();

            case 4:
                return null;
        }
    }(), null, function ($var2) {
        return function () {
            return {};
        }(function ($var1) {
            return function (arg00) {
                game.post(arg00);
            }(function (arg0) {
                return new AgentEvent(1, arg0);
            }($var1));
        }($var2));
    });
    return null;
});
window.addEventListener('keydown', function (x_1) {
    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["c" /* defaultArg */])(function () {
        var matchValue_1 = ~~x_1.which | 0;

        switch (matchValue_1) {
            case 38:
                return function () {
                    var inputRecord_4 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                    return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(true, inputRecord_4.Down, inputRecord_4.Left, inputRecord_4.Right);
                }();

            case 37:
                return function () {
                    var inputRecord_5 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                    return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(inputRecord_5.Up, inputRecord_5.Down, true, inputRecord_5.Right);
                }();

            case 39:
                return function () {
                    var inputRecord_6 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                    return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(inputRecord_6.Up, inputRecord_6.Down, inputRecord_6.Left, true);
                }();

            case 40:
                return function () {
                    var inputRecord_7 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                    return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(inputRecord_7.Up, true, inputRecord_7.Left, inputRecord_7.Right);
                }();

            default:
                return null;
        }
    }(), null, function ($var5) {
        return function () {
            return {};
        }(function ($var4) {
            return function (arg00_1) {
                game.post(arg00_1);
            }(function (arg0_2) {
                return new AgentEvent(1, arg0_2);
            }($var4));
        }($var5));
    });
    return null;
});
function touchEvents(startXY_0, startXY_1, endXY_0, endXY_1) {
    var startXY = [startXY_0, startXY_1];
    var endXY = [endXY_0, endXY_1];
    var body = document.body;

    var _Left_Right_Top_Down_Nothing_ = function _Left_Right_Top_Down_Nothing_(tupledArg) {
        var minHeightDiff = body.clientHeight * 0.07;
        var minWidthDiff = body.clientWidth * 0.07;
        var matchValue_2 = [tupledArg[1][0] - tupledArg[0][0], tupledArg[1][1] - tupledArg[0][1]];

        if (matchValue_2[0] > minWidthDiff) {
            return new __WEBPACK_IMPORTED_MODULE_7__node_modules_fable_core_Choice__["a" /* default */](1, null);
        } else if (matchValue_2[0] < -minWidthDiff) {
            return new __WEBPACK_IMPORTED_MODULE_7__node_modules_fable_core_Choice__["a" /* default */](0, null);
        } else if (matchValue_2[1] > minHeightDiff) {
            return new __WEBPACK_IMPORTED_MODULE_7__node_modules_fable_core_Choice__["a" /* default */](3, null);
        } else if (matchValue_2[1] < -minHeightDiff) {
            return new __WEBPACK_IMPORTED_MODULE_7__node_modules_fable_core_Choice__["a" /* default */](2, null);
        } else {
            return new __WEBPACK_IMPORTED_MODULE_7__node_modules_fable_core_Choice__["a" /* default */](4, null);
        }
    };

    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["c" /* defaultArg */])(function () {
        var matchValue_3 = [startXY, endXY];

        var activePatternResult327 = _Left_Right_Top_Down_Nothing_(matchValue_3);

        if (activePatternResult327.tag === 1) {
            return function () {
                var inputRecord_8 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(inputRecord_8.Up, inputRecord_8.Down, inputRecord_8.Left, true);
            }();
        } else if (activePatternResult327.tag === 2) {
            return function () {
                var inputRecord_9 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(true, inputRecord_9.Down, inputRecord_9.Left, inputRecord_9.Right);
            }();
        } else if (activePatternResult327.tag === 3) {
            return function () {
                var inputRecord_10 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(inputRecord_10.Up, true, inputRecord_10.Left, inputRecord_10.Right);
            }();
        } else if (activePatternResult327.tag === 4) {
            return null;
        } else {
            return function () {
                var inputRecord_11 = __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed.Default;
                return new __WEBPACK_IMPORTED_MODULE_2__Logic_fs__["a" /* Game */].KeyPressed(inputRecord_11.Up, inputRecord_11.Down, true, inputRecord_11.Right);
            }();
        }
    }(), null, function ($var7) {
        return function () {
            return {};
        }(function ($var6) {
            return function (arg00_2) {
                game.post(arg00_2);
            }(function (arg0_4) {
                return new AgentEvent(1, arg0_4);
            }($var6));
        }($var7));
    });
}
function setUpTouchEvents() {
    var startXY = [0, 0];
    var body = document.body;
    body.addEventListener('touchstart', function (x_2) {
        var touches = x_2.changedTouches[0];
        startXY = [touches.clientX, touches.clientY];
        return null;
    });
    body.addEventListener('touchend', function (x_3) {
        var touches_1 = x_3.changedTouches[0];
        __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__node_modules_fable_core_String__["a" /* fsFormat */])("%A - %A")(function (x) {
            console.log(x);
        })(startXY, [touches_1.clientX, touches_1.clientY]);
        var tupledArg = [touches_1.clientX, touches_1.clientY];
        var arg00_ = startXY[0];
        var arg01_ = startXY[1];
        touchEvents(arg00_, arg01_, tupledArg[0], tupledArg[1]);
        return null;
    });
}
setUpTouchEvents();
function init() {
    return function (builder_) {
        return builder_.Delay(function () {
            game.post(new AgentEvent(0));
            return builder_.Bind(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__node_modules_fable_core_Async__["a" /* sleep */])(500), function () {
                return builder_.ReturnFrom(init());
            });
        });
    }(__WEBPACK_IMPORTED_MODULE_6__node_modules_fable_core_AsyncBuilder__["a" /* singleton */]);
}

(function (arg00_2) {
    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_9__node_modules_fable_core_Async__["b" /* startImmediate */])(arg00_2);
})(init());

/***/ }),
/* 83 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator__ = __webpack_require__(13);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator__);
/* unused harmony export addRangeInPlace */
/* unused harmony export copyTo */
/* unused harmony export partition */
/* harmony export (immutable) */ __webpack_exports__["a"] = permute;
/* unused harmony export removeInPlace */
/* unused harmony export setSlice */
/* unused harmony export sortInPlaceBy */
/* unused harmony export unzip */
/* unused harmony export unzip3 */
/* unused harmony export getSubArray */
/* unused harmony export fill */

function addRangeInPlace(range, xs) {
    var iter = __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_get_iterator___default()(range);
    var cur = iter.next();
    while (!cur.done) {
        xs.push(cur.value);
        cur = iter.next();
    }
}
function copyTo(source, sourceIndex, target, targetIndex, count) {
    while (count--) {
        target[targetIndex++] = source[sourceIndex++];
    }
}
function partition(f, xs) {
    var ys = [],
        zs = [],
        j = 0,
        k = 0;
    for (var i = 0; i < xs.length; i++) {
        if (f(xs[i])) ys[j++] = xs[i];else zs[k++] = xs[i];
    }return [ys, zs];
}
function permute(f, xs) {
    var ys = xs.map(function () {
        return null;
    });
    var checkFlags = new Array(xs.length);
    for (var i = 0; i < xs.length; i++) {
        var j = f(i);
        if (j < 0 || j >= xs.length) throw new Error("Not a valid permutation");
        ys[j] = xs[i];
        checkFlags[j] = 1;
    }
    for (var _i = 0; _i < xs.length; _i++) {
        if (checkFlags[_i] != 1) throw new Error("Not a valid permutation");
    }return ys;
}
function removeInPlace(item, xs) {
    var i = xs.indexOf(item);
    if (i > -1) {
        xs.splice(i, 1);
        return true;
    }
    return false;
}
function setSlice(target, lower, upper, source) {
    var length = (upper || target.length - 1) - lower;
    if (ArrayBuffer.isView(target) && source.length <= length) target.set(source, lower);else for (var i = lower | 0, j = 0; j <= length; i++, j++) {
        target[i] = source[j];
    }
}
function sortInPlaceBy(f, xs) {
    var dir = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 1;

    return xs.sort(function (x, y) {
        x = f(x);
        y = f(y);
        return (x < y ? -1 : x == y ? 0 : 1) * dir;
    });
}
function unzip(xs) {
    var bs = new Array(xs.length),
        cs = new Array(xs.length);
    for (var i = 0; i < xs.length; i++) {
        bs[i] = xs[i][0];
        cs[i] = xs[i][1];
    }
    return [bs, cs];
}
function unzip3(xs) {
    var bs = new Array(xs.length),
        cs = new Array(xs.length),
        ds = new Array(xs.length);
    for (var i = 0; i < xs.length; i++) {
        bs[i] = xs[i][0];
        cs[i] = xs[i][1];
        ds[i] = xs[i][2];
    }
    return [bs, cs, ds];
}
function getSubArray(xs, startIndex, count) {
    return xs.slice(startIndex, startIndex + count);
}
function fill(target, targetIndex, count, value) {
    target.fill(value, targetIndex, targetIndex + count);
}

/***/ }),
/* 84 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator__ = __webpack_require__(13);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_array_from__ = __webpack_require__(31);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass__ = __webpack_require__(7);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__Symbol__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__Util__ = __webpack_require__(3);
/* unused harmony export ofArray */









function ofArray(args, base) {
    var acc = base || new List();
    for (var i = args.length - 1; i >= 0; i--) {
        acc = new List(args[i], acc);
    }
    return acc;
}

var List = function () {
    function List(head, tail) {
        __WEBPACK_IMPORTED_MODULE_3_babel_runtime_helpers_classCallCheck___default()(this, List);

        this.head = head;
        this.tail = tail;
    }

    __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_createClass___default()(List, [{
        key: "ToString",
        value: function ToString() {
            return "[" + __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_array_from___default()(this).map(__WEBPACK_IMPORTED_MODULE_6__Util__["e" /* toString */]).join("; ") + "]";
        }
    }, {
        key: "Equals",
        value: function Equals(x) {
            if (this === x) {
                return true;
            } else {
                var iter1 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(this),
                    iter2 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(x);
                for (;;) {
                    var cur1 = iter1.next(),
                        cur2 = iter2.next();
                    if (cur1.done) return cur2.done ? true : false;else if (cur2.done) return false;else if (!__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__Util__["a" /* equals */])(cur1.value, cur2.value)) return false;
                }
            }
        }
    }, {
        key: "CompareTo",
        value: function CompareTo(x) {
            if (this === x) {
                return 0;
            } else {
                var acc = 0;
                var iter1 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(this),
                    iter2 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_get_iterator___default()(x);
                for (;;) {
                    var cur1 = iter1.next(),
                        cur2 = iter2.next();
                    if (cur1.done) return cur2.done ? acc : -1;else if (cur2.done) return 1;else {
                        acc = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__Util__["d" /* compare */])(cur1.value, cur2.value);
                        if (acc != 0) return acc;
                    }
                }
            }
        }
    }, {
        key: __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_symbol_iterator___default.a,
        value: function value() {
            var cur = this;
            return {
                next: function next() {
                    var tmp = cur;
                    cur = cur.tail;
                    return { done: tmp.tail == null, value: tmp.head };
                }
            };
        }
    }, {
        key: __WEBPACK_IMPORTED_MODULE_5__Symbol__["a" /* default */].reflection,
        value: function value() {
            return {
                type: "Microsoft.FSharp.Collections.FSharpList",
                interfaces: ["System.IEquatable", "System.IComparable"]
            };
        }
    }, {
        key: "length",
        get: function get() {
            var cur = this,
                acc = 0;
            while (cur.tail != null) {
                cur = cur.tail;
                acc++;
            }
            return acc;
        }
    }]);

    return List;
}();

/* harmony default export */ __webpack_exports__["a"] = (List);

/***/ }),
/* 85 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_createClass__ = __webpack_require__(7);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__Async__ = __webpack_require__(29);
/* harmony export (immutable) */ __webpack_exports__["a"] = start;






var QueueCell = function QueueCell(message) {
    __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, QueueCell);

    this.value = message;
};

var MailboxQueue = function () {
    function MailboxQueue() {
        __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, MailboxQueue);
    }

    __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_createClass___default()(MailboxQueue, [{
        key: "add",
        value: function add(message) {
            var itCell = new QueueCell(message);
            if (this.firstAndLast) {
                this.firstAndLast[1].next = itCell;
                this.firstAndLast = [this.firstAndLast[0], itCell];
            } else this.firstAndLast = [itCell, itCell];
        }
    }, {
        key: "tryGet",
        value: function tryGet() {
            if (this.firstAndLast) {
                var value = this.firstAndLast[0].value;
                if (this.firstAndLast[0].next) this.firstAndLast = [this.firstAndLast[0].next, this.firstAndLast[1]];else delete this.firstAndLast;
                return value;
            }
            return void 0;
        }
    }]);

    return MailboxQueue;
}();

var MailboxProcessor = function () {
    function MailboxProcessor(body, cancellationToken) {
        __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, MailboxProcessor);

        this.body = body;
        this.cancellationToken = cancellationToken || __WEBPACK_IMPORTED_MODULE_2__Async__["c" /* defaultCancellationToken */];
        this.messages = new MailboxQueue();
    }

    __WEBPACK_IMPORTED_MODULE_0_babel_runtime_helpers_createClass___default()(MailboxProcessor, [{
        key: "__processEvents",
        value: function __processEvents() {
            if (this.continuation) {
                var value = this.messages.tryGet();
                if (value) {
                    var cont = this.continuation;
                    delete this.continuation;
                    cont(value);
                }
            }
        }
    }, {
        key: "start",
        value: function start() {
            __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Async__["b" /* startImmediate */])(this.body(this), this.cancellationToken);
        }
    }, {
        key: "receive",
        value: function receive() {
            var _this = this;

            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Async__["d" /* fromContinuations */])(function (conts) {
                if (_this.continuation) throw new Error("Receive can only be called once!");
                _this.continuation = conts[0];
                _this.__processEvents();
            });
        }
    }, {
        key: "post",
        value: function post(message) {
            this.messages.add(message);
            this.__processEvents();
        }
    }, {
        key: "postAndAsyncReply",
        value: function postAndAsyncReply(buildMessage) {
            var result = void 0;
            var continuation = void 0;
            function checkCompletion() {
                if (result && continuation) continuation(result);
            }
            var reply = {
                reply: function reply(res) {
                    result = res;
                    checkCompletion();
                }
            };
            this.messages.add(buildMessage(reply));
            this.__processEvents();
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_2__Async__["d" /* fromContinuations */])(function (conts) {
                continuation = conts[0];
                checkCompletion();
            });
        }
    }]);

    return MailboxProcessor;
}();

/* unused harmony default export */ var _unused_webpack_default_export = (MailboxProcessor);

function start(body, cancellationToken) {
    var mbox = new MailboxProcessor(body, cancellationToken);
    mbox.start();
    return mbox;
}

/***/ }),
/* 86 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* unused harmony export create */
/* harmony export (immutable) */ __webpack_exports__["a"] = escape;
/* unused harmony export unescape */
/* unused harmony export isMatch */
/* unused harmony export match */
/* unused harmony export matches */
/* unused harmony export options */
/* unused harmony export replace */
/* unused harmony export split */
function create(pattern, options) {
    var flags = "g";
    flags += options & 1 ? "i" : "";
    flags += options & 2 ? "m" : "";
    return new RegExp(pattern, flags);
}
function escape(str) {
    return str.replace(/[\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
}
function unescape(str) {
    return str.replace(/\\([\-\[\/\{\}\(\)\*\+\?\.\\\^\$\|])/g, "$1");
}
function isMatch(str, pattern) {
    var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;

    var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create(pattern, options);
    return reg.test(str);
}
function match(str, pattern) {
    var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;

    var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create(pattern, options);
    return reg.exec(str);
}
function matches(str, pattern) {
    var options = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;

    var reg = str instanceof RegExp ? (reg = str, str = pattern, reg.lastIndex = options, reg) : reg = create(pattern, options);
    if (!reg.global) throw new Error("Non-global RegExp");
    var m = void 0;
    var matches = [];
    while ((m = reg.exec(str)) !== null) {
        matches.push(m);
    }return matches;
}
function options(reg) {
    var options = 256;
    options |= reg.ignoreCase ? 1 : 0;
    options |= reg.multiline ? 2 : 0;
    return options;
}
function replace(reg, input, replacement, limit) {
    var offset = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;

    function replacer() {
        var res = arguments[0];
        if (limit !== 0) {
            limit--;
            var _match = [];
            var len = arguments.length;
            for (var i = 0; i < len - 2; i++) {
                _match.push(arguments[i]);
            }_match.index = arguments[len - 2];
            _match.input = arguments[len - 1];
            res = replacement(_match);
        }
        return res;
    }
    if (typeof reg == "string") {
        var tmp = reg;
        reg = create(input, limit);
        input = tmp;
        limit = undefined;
    }
    if (typeof replacement == "function") {
        limit = limit == null ? -1 : limit;
        return input.substring(0, offset) + input.substring(offset).replace(reg, replacer);
    } else {
        if (limit != null) {
            var m = void 0;
            var sub1 = input.substring(offset);
            var _matches = matches(reg, sub1);
            var sub2 = matches.length > limit ? (m = _matches[limit - 1], sub1.substring(0, m.index + m[0].length)) : sub1;
            return input.substring(0, offset) + sub2.replace(reg, replacement) + input.substring(offset + sub2.length);
        } else {
            return input.replace(reg, replacement);
        }
    }
}
function split(reg, input, limit) {
    var offset = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;

    if (typeof reg == "string") {
        var tmp = reg;
        reg = create(input, limit);
        input = tmp;
        limit = undefined;
    }
    input = input.substring(offset);
    return input.split(reg, limit);
}

/***/ }),
/* 87 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_names__ = __webpack_require__(63);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_names___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_names__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from__ = __webpack_require__(31);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_symbol_iterator__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_symbol_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_symbol_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_json_stringify__ = __webpack_require__(60);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_json_stringify___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_json_stringify__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_typeof__ = __webpack_require__(66);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_typeof___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_typeof__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_get_iterator__ = __webpack_require__(13);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_get_iterator___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_get_iterator__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__Util__ = __webpack_require__(3);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_7__RegExp__ = __webpack_require__(86);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_8__Date__ = __webpack_require__(18);
/* unused harmony export compare */
/* unused harmony export compareTo */
/* unused harmony export startsWith */
/* unused harmony export indexOfAny */
/* harmony export (immutable) */ __webpack_exports__["a"] = fsFormat;
/* unused harmony export format */
/* unused harmony export endsWith */
/* unused harmony export initialize */
/* unused harmony export insert */
/* unused harmony export isNullOrEmpty */
/* unused harmony export isNullOrWhiteSpace */
/* unused harmony export join */
/* unused harmony export newGuid */
/* unused harmony export padLeft */
/* unused harmony export padRight */
/* unused harmony export remove */
/* unused harmony export replace */
/* unused harmony export replicate */
/* unused harmony export split */
/* unused harmony export trim */














var fsFormatRegExp = /(^|[^%])%([0+ ]*)(-?\d+)?(?:\.(\d+))?(\w)/;
var formatRegExp = /\{(\d+)(,-?\d+)?(?:\:(.+?))?\}/g;
var StringComparison = {
    CurrentCulture: 0,
    CurrentCultureIgnoreCase: 1,
    InvariantCulture: 2,
    InvariantCultureIgnoreCase: 3,
    Ordinal: 4,
    OrdinalIgnoreCase: 5
};
function cmp(x, y, ic) {
    function isIgnoreCase(i) {
        return i === true || i === StringComparison.CurrentCultureIgnoreCase || i === StringComparison.InvariantCultureIgnoreCase || i === StringComparison.OrdinalIgnoreCase;
    }
    function isOrdinal(i) {
        return i === StringComparison.Ordinal || i === StringComparison.OrdinalIgnoreCase;
    }
    if (x == null) return y == null ? 0 : -1;
    if (y == null) return 1;
    if (isOrdinal(ic)) {
        if (isIgnoreCase(ic)) {
            x = x.toLowerCase();
            y = y.toLowerCase();
        }
        return x === y ? 0 : x < y ? -1 : 1;
    } else {
        if (isIgnoreCase(ic)) {
            x = x.toLocaleLowerCase();
            y = y.toLocaleLowerCase();
        }
        return x.localeCompare(y);
    }
}
function compare() {
    for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
    }

    switch (args.length) {
        case 2:
            return cmp(args[0], args[1], false);
        case 3:
            return cmp(args[0], args[1], args[2]);
        case 4:
            return cmp(args[0], args[1], args[2] === true);
        case 5:
            return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), false);
        case 6:
            return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), args[5]);
        case 7:
            return cmp(args[0].substr(args[1], args[4]), args[2].substr(args[3], args[4]), args[5] === true);
        default:
            throw new Error("String.compare: Unsupported number of parameters");
    }
}
function compareTo(x, y) {
    return cmp(x, y, false);
}
function startsWith(str, pattern, ic) {
    if (str.length >= pattern.length) {
        return cmp(str.substr(0, pattern.length), pattern, ic) === 0;
    }
    return false;
}
function indexOfAny(str, anyOf) {
    if (str == null || str === "") return -1;
    var startIndex = (arguments.length <= 2 ? 0 : arguments.length - 2) > 0 ? arguments.length <= 2 ? undefined : arguments[2] : 0;
    if (startIndex < 0) throw new Error("String.indexOfAny: Start index cannot be negative");
    var length = (arguments.length <= 2 ? 0 : arguments.length - 2) > 1 ? arguments.length <= 3 ? undefined : arguments[3] : str.length - startIndex;
    if (length < 0) throw new Error("String.indexOfAny: Length cannot be negative");
    if (length > str.length - startIndex) throw new Error("String.indexOfAny: Invalid startIndex and length");
    str = str.substr(startIndex, length);
    var _iteratorNormalCompletion = true;
    var _didIteratorError = false;
    var _iteratorError = undefined;

    try {
        for (var _iterator = __WEBPACK_IMPORTED_MODULE_5_babel_runtime_core_js_get_iterator___default()(anyOf), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
            var c = _step.value;

            var index = str.indexOf(c);
            if (index > -1) return index + startIndex;
        }
    } catch (err) {
        _didIteratorError = true;
        _iteratorError = err;
    } finally {
        try {
            if (!_iteratorNormalCompletion && _iterator.return) {
                _iterator.return();
            }
        } finally {
            if (_didIteratorError) {
                throw _iteratorError;
            }
        }
    }

    return -1;
}
function toHex(value) {
    return value < 0 ? "ff" + (16777215 - (Math.abs(value) - 1)).toString(16) : value.toString(16);
}
function fsFormat(str) {
    for (var _len2 = arguments.length, args = Array(_len2 > 1 ? _len2 - 1 : 0), _key2 = 1; _key2 < _len2; _key2++) {
        args[_key2 - 1] = arguments[_key2];
    }

    function isObject(x) {
        return x !== null && (typeof x === "undefined" ? "undefined" : __WEBPACK_IMPORTED_MODULE_4_babel_runtime_helpers_typeof___default()(x)) === "object" && !(x instanceof Number) && !(x instanceof String) && !(x instanceof Boolean);
    }
    function formatOnce(str, rep) {
        return str.replace(fsFormatRegExp, function (_, prefix, flags, pad, precision, format) {
            switch (format) {
                case "f":
                case "F":
                    rep = rep.toFixed(precision || 6);
                    break;
                case "g":
                case "G":
                    rep = rep.toPrecision(precision);
                    break;
                case "e":
                case "E":
                    rep = rep.toExponential(precision);
                    break;
                case "O":
                    rep = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__Util__["e" /* toString */])(rep);
                    break;
                case "A":
                    try {
                        rep = __WEBPACK_IMPORTED_MODULE_3_babel_runtime_core_js_json_stringify___default()(rep, function (k, v) {
                            return v && v[__WEBPACK_IMPORTED_MODULE_2_babel_runtime_core_js_symbol_iterator___default.a] && !Array.isArray(v) && isObject(v) ? __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(v) : v && typeof v.ToString === "function" ? __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_6__Util__["e" /* toString */])(v) : v;
                        });
                    } catch (err) {
                        rep = "{" + __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_object_get_own_property_names___default()(rep).map(function (k) {
                            return k + ": " + String(rep[k]);
                        }).join(", ") + "}";
                    }
                    break;
                case "x":
                    rep = toHex(Number(rep));
                    break;
                case "X":
                    rep = toHex(Number(rep)).toUpperCase();
                    break;
            }
            var plusPrefix = flags.indexOf("+") >= 0 && parseInt(rep) >= 0;
            if (!isNaN(pad = parseInt(pad))) {
                var ch = pad >= 0 && flags.indexOf("0") >= 0 ? "0" : " ";
                rep = padLeft(rep, Math.abs(pad) - (plusPrefix ? 1 : 0), ch, pad < 0);
            }
            var once = prefix + (plusPrefix ? "+" + rep : rep);
            return once.replace(/%/g, "%%");
        });
    }
    if (args.length === 0) {
        return function (cont) {
            if (fsFormatRegExp.test(str)) {
                return function () {
                    var strCopy = str;
                    for (var i = 0; i < arguments.length; i++) {
                        strCopy = formatOnce(strCopy, arguments[i]);
                    }
                    return cont(strCopy.replace(/%%/g, "%"));
                };
            } else {
                return cont(str);
            }
        };
    } else {
        for (var i = 0; i < args.length; i++) {
            str = formatOnce(str, args[i]);
        }
        return str.replace(/%%/g, "%");
    }
}
function format(str) {
    for (var _len3 = arguments.length, args = Array(_len3 > 1 ? _len3 - 1 : 0), _key3 = 1; _key3 < _len3; _key3++) {
        args[_key3 - 1] = arguments[_key3];
    }

    return str.replace(formatRegExp, function (match, idx, pad, format) {
        var rep = args[idx],
            padSymbol = " ";
        if (typeof rep === "number") {
            switch ((format || "").substring(0, 1)) {
                case "f":
                case "F":
                    rep = format.length > 1 ? rep.toFixed(format.substring(1)) : rep.toFixed(2);
                    break;
                case "g":
                case "G":
                    rep = format.length > 1 ? rep.toPrecision(format.substring(1)) : rep.toPrecision();
                    break;
                case "e":
                case "E":
                    rep = format.length > 1 ? rep.toExponential(format.substring(1)) : rep.toExponential();
                    break;
                case "p":
                case "P":
                    rep = (format.length > 1 ? (rep * 100).toFixed(format.substring(1)) : (rep * 100).toFixed(2)) + " %";
                    break;
                case "x":
                    rep = toHex(Number(rep));
                    break;
                case "X":
                    rep = toHex(Number(rep)).toUpperCase();
                    break;
                default:
                    var m = /^(0+)(\.0+)?$/.exec(format);
                    if (m != null) {
                        var decs = 0;
                        if (m[2] != null) rep = rep.toFixed(decs = m[2].length - 1);
                        pad = "," + (m[1].length + (decs ? decs + 1 : 0)).toString();
                        padSymbol = "0";
                    } else if (format) {
                        rep = format;
                    }
            }
        } else if (rep instanceof Date) {
            if (format.length === 1) {
                switch (format) {
                    case "D":
                        rep = rep.toDateString();
                        break;
                    case "T":
                        rep = rep.toLocaleTimeString();
                        break;
                    case "d":
                        rep = rep.toLocaleDateString();
                        break;
                    case "t":
                        rep = rep.toLocaleTimeString().replace(/:\d\d(?!:)/, "");
                        break;
                    case "o":
                    case "O":
                        if (rep.kind === 2) {
                            var offset = rep.getTimezoneOffset() * -1;
                            rep = format("{0:yyyy-MM-dd}T{0:HH:mm}:{1:00.000}{2}{3:00}:{4:00}", rep, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["a" /* second */])(rep), offset >= 0 ? "+" : "-", ~~(offset / 60), offset % 60);
                        } else {
                            rep = rep.toISOString();
                        }
                }
            } else {
                rep = format.replace(/\w+/g, function (match2) {
                    var rep2 = match2;
                    switch (match2.substring(0, 1)) {
                        case "y":
                            rep2 = match2.length < 4 ? __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["b" /* year */])(rep) % 100 : __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["b" /* year */])(rep);
                            break;
                        case "h":
                            rep2 = rep.getHours() > 12 ? __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["c" /* hour */])(rep) % 12 : __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["c" /* hour */])(rep);
                            break;
                        case "M":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["d" /* month */])(rep);
                            break;
                        case "d":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["e" /* day */])(rep);
                            break;
                        case "H":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["c" /* hour */])(rep);
                            break;
                        case "m":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["f" /* minute */])(rep);
                            break;
                        case "s":
                            rep2 = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_8__Date__["a" /* second */])(rep);
                            break;
                    }
                    if (rep2 !== match2 && rep2 < 10 && match2.length > 1) {
                        rep2 = "0" + rep2;
                    }
                    return rep2;
                });
            }
        }
        if (!isNaN(pad = parseInt((pad || "").substring(1)))) {
            rep = padLeft(rep, Math.abs(pad), padSymbol, pad < 0);
        }
        return rep;
    });
}
function endsWith(str, search) {
    var idx = str.lastIndexOf(search);
    return idx >= 0 && idx == str.length - search.length;
}
function initialize(n, f) {
    if (n < 0) throw new Error("String length must be non-negative");
    var xs = new Array(n);
    for (var i = 0; i < n; i++) {
        xs[i] = f(i);
    }return xs.join("");
}
function insert(str, startIndex, value) {
    if (startIndex < 0 || startIndex > str.length) {
        throw new Error("startIndex is negative or greater than the length of this instance.");
    }
    return str.substring(0, startIndex) + value + str.substring(startIndex);
}
function isNullOrEmpty(str) {
    return typeof str !== "string" || str.length == 0;
}
function isNullOrWhiteSpace(str) {
    return typeof str !== "string" || /^\s*$/.test(str);
}
function join(delimiter, xs) {
    var xs2 = xs;
    if (typeof xs === "string") {
        var len = arguments.length;
        xs2 = Array(len - 1);
        for (var key = 1; key < len; key++) {
            xs2[key - 1] = arguments[key];
        }
    } else if (!Array.isArray(xs)) {
        xs2 = __WEBPACK_IMPORTED_MODULE_1_babel_runtime_core_js_array_from___default()(xs);
    }
    return xs2.join(delimiter);
}
function newGuid() {
    var uuid = "";
    for (var i = 0; i < 32; i++) {
        var random = Math.random() * 16 | 0;
        if (i === 8 || i === 12 || i === 16 || i === 20) uuid += "-";
        uuid += (i === 12 ? 4 : i === 16 ? random & 3 | 8 : random).toString(16);
    }
    return uuid;
}
function padLeft(str, len, ch, isRight) {
    ch = ch || " ";
    str = String(str);
    len = len - str.length;
    for (var i = -1; ++i < len;) {
        str = isRight ? str + ch : ch + str;
    }return str;
}
function padRight(str, len, ch) {
    return padLeft(str, len, ch, true);
}
function remove(str, startIndex, count) {
    if (startIndex >= str.length) {
        throw new Error("startIndex must be less than length of string");
    }
    if (typeof count === "number" && startIndex + count > str.length) {
        throw new Error("Index and count must refer to a location within the string.");
    }
    return str.slice(0, startIndex) + (typeof count === "number" ? str.substr(startIndex + count) : "");
}
function replace(str, search, replace) {
    return str.replace(new RegExp(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__RegExp__["a" /* escape */])(search), "g"), replace);
}
function replicate(n, x) {
    return initialize(n, function () {
        return x;
    });
}
function split(str, splitters, count, removeEmpty) {
    count = typeof count == "number" ? count : null;
    removeEmpty = typeof removeEmpty == "number" ? removeEmpty : null;
    if (count < 0) throw new Error("Count cannot be less than zero");
    if (count === 0) return [];
    var splitters2 = splitters;
    if (!Array.isArray(splitters)) {
        var len = arguments.length;
        splitters2 = Array(len - 1);
        for (var key = 1; key < len; key++) {
            splitters2[key - 1] = arguments[key];
        }
    }
    splitters2 = splitters2.map(function (x) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__RegExp__["a" /* escape */])(x);
    });
    splitters2 = splitters2.length > 0 ? splitters2 : [" "];
    var m = void 0;
    var i = 0;
    var splits = [];
    var reg = new RegExp(splitters2.join("|"), "g");
    while ((count == null || count > 1) && (m = reg.exec(str)) !== null) {
        if (!removeEmpty || m.index - i > 0) {
            count = count != null ? count - 1 : count;
            splits.push(str.substring(i, m.index));
        }
        i = reg.lastIndex;
    }
    if (!removeEmpty || str.length - i > 0) splits.push(str.substring(i));
    return splits;
}
function trim(str, side) {
    for (var _len4 = arguments.length, chars = Array(_len4 > 2 ? _len4 - 2 : 0), _key4 = 2; _key4 < _len4; _key4++) {
        chars[_key4 - 2] = arguments[_key4];
    }

    if (side == "both" && chars.length == 0) return str.trim();
    if (side == "start" || side == "both") {
        var reg = chars.length == 0 ? /^\s+/ : new RegExp("^[" + __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__RegExp__["a" /* escape */])(chars.join("")) + "]+");
        str = str.replace(reg, "");
    }
    if (side == "end" || side == "both") {
        var _reg = chars.length == 0 ? /\s+$/ : new RegExp("[" + __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_7__RegExp__["a" /* escape */])(chars.join("")) + "]+$");
        str = str.replace(_reg, "");
    }
    return str;
}

/***/ }),
/* 88 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__Util__ = __webpack_require__(3);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__Long__ = __webpack_require__(58);
/* harmony export (immutable) */ __webpack_exports__["a"] = create;
/* unused harmony export fromTicks */
/* unused harmony export fromDays */
/* unused harmony export fromHours */
/* unused harmony export fromMinutes */
/* unused harmony export fromSeconds */
/* unused harmony export days */
/* unused harmony export hours */
/* unused harmony export minutes */
/* unused harmony export seconds */
/* unused harmony export milliseconds */
/* unused harmony export ticks */
/* unused harmony export totalDays */
/* unused harmony export totalHours */
/* unused harmony export totalMinutes */
/* unused harmony export totalSeconds */
/* unused harmony export negate */
/* unused harmony export add */
/* unused harmony export subtract */
/* unused harmony export compare */
/* unused harmony export compareTo */
/* unused harmony export duration */


function create() {
    var d = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 0;
    var h = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
    var m = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 0;
    var s = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 0;
    var ms = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : 0;

    switch (arguments.length) {
        case 1:
            return fromTicks(arguments[0]);
        case 3:
            d = 0, h = arguments[0], m = arguments[1], s = arguments[2], ms = 0;
            break;
        default:
            d = arguments[0], h = arguments[1], m = arguments[2], s = arguments[3], ms = arguments[4] | 0;
            break;
    }
    return d * 86400000 + h * 3600000 + m * 60000 + s * 1000 + ms;
}
function fromTicks(ticks) {
    return ticks.div(10000).toNumber();
}
function fromDays(d) {
    return create(d, 0, 0, 0);
}
function fromHours(h) {
    return create(h, 0, 0);
}
function fromMinutes(m) {
    return create(0, m, 0);
}
function fromSeconds(s) {
    return create(0, 0, s);
}
function days(ts) {
    return Math.floor(ts / 86400000);
}
function hours(ts) {
    return Math.floor(ts % 86400000 / 3600000);
}
function minutes(ts) {
    return Math.floor(ts % 3600000 / 60000);
}
function seconds(ts) {
    return Math.floor(ts % 60000 / 1000);
}
function milliseconds(ts) {
    return Math.floor(ts % 1000);
}
function ticks(ts) {
    return __WEBPACK_IMPORTED_MODULE_1__Long__["a" /* fromNumber */](ts).mul(10000);
}
function totalDays(ts) {
    return ts / 86400000;
}
function totalHours(ts) {
    return ts / 3600000;
}
function totalMinutes(ts) {
    return ts / 60000;
}
function totalSeconds(ts) {
    return ts / 1000;
}
function negate(ts) {
    return ts * -1;
}
function add(ts1, ts2) {
    return ts1 + ts2;
}
function subtract(ts1, ts2) {
    return ts1 - ts2;
}
function compare(x, y) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_0__Util__["d" /* compare */])(x, y);
}
function compareTo(x, y) {
    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_0__Util__["d" /* compare */])(x, y);
}
function duration(x) {
    return Math.abs(x);
}

/***/ }),
/* 89 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(97), __esModule: true };

/***/ }),
/* 90 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(100), __esModule: true };

/***/ }),
/* 91 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(102), __esModule: true };

/***/ }),
/* 92 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(104), __esModule: true };

/***/ }),
/* 93 */
/***/ (function(module, exports, __webpack_require__) {

module.exports = { "default": __webpack_require__(105), __esModule: true };

/***/ }),
/* 94 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


exports.__esModule = true;

var _isIterable2 = __webpack_require__(89);

var _isIterable3 = _interopRequireDefault(_isIterable2);

var _getIterator2 = __webpack_require__(13);

var _getIterator3 = _interopRequireDefault(_getIterator2);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.default = function () {
  function sliceIterator(arr, i) {
    var _arr = [];
    var _n = true;
    var _d = false;
    var _e = undefined;

    try {
      for (var _i = (0, _getIterator3.default)(arr), _s; !(_n = (_s = _i.next()).done); _n = true) {
        _arr.push(_s.value);

        if (i && _arr.length === i) break;
      }
    } catch (err) {
      _d = true;
      _e = err;
    } finally {
      try {
        if (!_n && _i["return"]) _i["return"]();
      } finally {
        if (_d) throw _e;
      }
    }

    return _arr;
  }

  return function (arr, i) {
    if (Array.isArray(arr)) {
      return arr;
    } else if ((0, _isIterable3.default)(Object(arr))) {
      return sliceIterator(arr, i);
    } else {
      throw new TypeError("Invalid attempt to destructure non-iterable instance");
    }
  };
}();

/***/ }),
/* 95 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(20);
__webpack_require__(130);
module.exports = __webpack_require__(0).Array.from;

/***/ }),
/* 96 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(28);
__webpack_require__(20);
module.exports = __webpack_require__(128);

/***/ }),
/* 97 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(28);
__webpack_require__(20);
module.exports = __webpack_require__(129);

/***/ }),
/* 98 */
/***/ (function(module, exports, __webpack_require__) {

var core  = __webpack_require__(0)
  , $JSON = core.JSON || (core.JSON = {stringify: JSON.stringify});
module.exports = function stringify(it){ // eslint-disable-line no-unused-vars
  return $JSON.stringify.apply($JSON, arguments);
};

/***/ }),
/* 99 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(57);
__webpack_require__(20);
__webpack_require__(28);
__webpack_require__(132);
__webpack_require__(140);
module.exports = __webpack_require__(0).Map;

/***/ }),
/* 100 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(133);
module.exports = __webpack_require__(0).Object.assign;

/***/ }),
/* 101 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(134);
var $Object = __webpack_require__(0).Object;
module.exports = function defineProperty(it, key, desc){
  return $Object.defineProperty(it, key, desc);
};

/***/ }),
/* 102 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(135);
var $Object = __webpack_require__(0).Object;
module.exports = function getOwnPropertyDescriptor(it, key){
  return $Object.getOwnPropertyDescriptor(it, key);
};

/***/ }),
/* 103 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(136);
var $Object = __webpack_require__(0).Object;
module.exports = function getOwnPropertyNames(it){
  return $Object.getOwnPropertyNames(it);
};

/***/ }),
/* 104 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(137);
module.exports = __webpack_require__(0).Object.getPrototypeOf;

/***/ }),
/* 105 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(57);
__webpack_require__(20);
__webpack_require__(28);
__webpack_require__(138);
module.exports = __webpack_require__(0).Promise;

/***/ }),
/* 106 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(139);
__webpack_require__(57);
__webpack_require__(141);
__webpack_require__(142);
module.exports = __webpack_require__(0).Symbol;

/***/ }),
/* 107 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(20);
__webpack_require__(28);
module.exports = __webpack_require__(55).f('iterator');

/***/ }),
/* 108 */
/***/ (function(module, exports) {

module.exports = function(){ /* empty */ };

/***/ }),
/* 109 */
/***/ (function(module, exports, __webpack_require__) {

var forOf = __webpack_require__(34);

module.exports = function(iter, ITERATOR){
  var result = [];
  forOf(iter, false, result.push, result, ITERATOR);
  return result;
};


/***/ }),
/* 110 */
/***/ (function(module, exports, __webpack_require__) {

// false -> Array#indexOf
// true  -> Array#includes
var toIObject = __webpack_require__(12)
  , toLength  = __webpack_require__(37)
  , toIndex   = __webpack_require__(127);
module.exports = function(IS_INCLUDES){
  return function($this, el, fromIndex){
    var O      = toIObject($this)
      , length = toLength(O.length)
      , index  = toIndex(fromIndex, length)
      , value;
    // Array#includes uses SameValueZero equality algorithm
    if(IS_INCLUDES && el != el)while(length > index){
      value = O[index++];
      if(value != value)return true;
    // Array#toIndex ignores holes, Array#includes - not
    } else for(;length > index; index++)if(IS_INCLUDES || index in O){
      if(O[index] === el)return IS_INCLUDES || index || 0;
    } return !IS_INCLUDES && -1;
  };
};

/***/ }),
/* 111 */
/***/ (function(module, exports, __webpack_require__) {

// 0 -> Array#forEach
// 1 -> Array#map
// 2 -> Array#filter
// 3 -> Array#some
// 4 -> Array#every
// 5 -> Array#find
// 6 -> Array#findIndex
var ctx      = __webpack_require__(14)
  , IObject  = __webpack_require__(43)
  , toObject = __webpack_require__(27)
  , toLength = __webpack_require__(37)
  , asc      = __webpack_require__(113);
module.exports = function(TYPE, $create){
  var IS_MAP        = TYPE == 1
    , IS_FILTER     = TYPE == 2
    , IS_SOME       = TYPE == 3
    , IS_EVERY      = TYPE == 4
    , IS_FIND_INDEX = TYPE == 6
    , NO_HOLES      = TYPE == 5 || IS_FIND_INDEX
    , create        = $create || asc;
  return function($this, callbackfn, that){
    var O      = toObject($this)
      , self   = IObject(O)
      , f      = ctx(callbackfn, that, 3)
      , length = toLength(self.length)
      , index  = 0
      , result = IS_MAP ? create($this, length) : IS_FILTER ? create($this, 0) : undefined
      , val, res;
    for(;length > index; index++)if(NO_HOLES || index in self){
      val = self[index];
      res = f(val, index, O);
      if(TYPE){
        if(IS_MAP)result[index] = res;            // map
        else if(res)switch(TYPE){
          case 3: return true;                    // some
          case 5: return val;                     // find
          case 6: return index;                   // findIndex
          case 2: result.push(val);               // filter
        } else if(IS_EVERY)return false;          // every
      }
    }
    return IS_FIND_INDEX ? -1 : IS_SOME || IS_EVERY ? IS_EVERY : result;
  };
};

/***/ }),
/* 112 */
/***/ (function(module, exports, __webpack_require__) {

var isObject = __webpack_require__(17)
  , isArray  = __webpack_require__(70)
  , SPECIES  = __webpack_require__(1)('species');

module.exports = function(original){
  var C;
  if(isArray(original)){
    C = original.constructor;
    // cross-realm fallback
    if(typeof C == 'function' && (C === Array || isArray(C.prototype)))C = undefined;
    if(isObject(C)){
      C = C[SPECIES];
      if(C === null)C = undefined;
    }
  } return C === undefined ? Array : C;
};

/***/ }),
/* 113 */
/***/ (function(module, exports, __webpack_require__) {

// 9.4.2.3 ArraySpeciesCreate(originalArray, length)
var speciesConstructor = __webpack_require__(112);

module.exports = function(original, length){
  return new (speciesConstructor(original))(length);
};

/***/ }),
/* 114 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var dP          = __webpack_require__(4).f
  , create      = __webpack_require__(46)
  , redefineAll = __webpack_require__(49)
  , ctx         = __webpack_require__(14)
  , anInstance  = __webpack_require__(40)
  , defined     = __webpack_require__(33)
  , forOf       = __webpack_require__(34)
  , $iterDefine = __webpack_require__(44)
  , step        = __webpack_require__(73)
  , setSpecies  = __webpack_require__(80)
  , DESCRIPTORS = __webpack_require__(6)
  , fastKey     = __webpack_require__(45).fastKey
  , SIZE        = DESCRIPTORS ? '_s' : 'size';

var getEntry = function(that, key){
  // fast case
  var index = fastKey(key), entry;
  if(index !== 'F')return that._i[index];
  // frozen object case
  for(entry = that._f; entry; entry = entry.n){
    if(entry.k == key)return entry;
  }
};

module.exports = {
  getConstructor: function(wrapper, NAME, IS_MAP, ADDER){
    var C = wrapper(function(that, iterable){
      anInstance(that, C, NAME, '_i');
      that._i = create(null); // index
      that._f = undefined;    // first entry
      that._l = undefined;    // last entry
      that[SIZE] = 0;         // size
      if(iterable != undefined)forOf(iterable, IS_MAP, that[ADDER], that);
    });
    redefineAll(C.prototype, {
      // 23.1.3.1 Map.prototype.clear()
      // 23.2.3.2 Set.prototype.clear()
      clear: function clear(){
        for(var that = this, data = that._i, entry = that._f; entry; entry = entry.n){
          entry.r = true;
          if(entry.p)entry.p = entry.p.n = undefined;
          delete data[entry.i];
        }
        that._f = that._l = undefined;
        that[SIZE] = 0;
      },
      // 23.1.3.3 Map.prototype.delete(key)
      // 23.2.3.4 Set.prototype.delete(value)
      'delete': function(key){
        var that  = this
          , entry = getEntry(that, key);
        if(entry){
          var next = entry.n
            , prev = entry.p;
          delete that._i[entry.i];
          entry.r = true;
          if(prev)prev.n = next;
          if(next)next.p = prev;
          if(that._f == entry)that._f = next;
          if(that._l == entry)that._l = prev;
          that[SIZE]--;
        } return !!entry;
      },
      // 23.2.3.6 Set.prototype.forEach(callbackfn, thisArg = undefined)
      // 23.1.3.5 Map.prototype.forEach(callbackfn, thisArg = undefined)
      forEach: function forEach(callbackfn /*, that = undefined */){
        anInstance(this, C, 'forEach');
        var f = ctx(callbackfn, arguments.length > 1 ? arguments[1] : undefined, 3)
          , entry;
        while(entry = entry ? entry.n : this._f){
          f(entry.v, entry.k, this);
          // revert to the last existing entry
          while(entry && entry.r)entry = entry.p;
        }
      },
      // 23.1.3.7 Map.prototype.has(key)
      // 23.2.3.7 Set.prototype.has(value)
      has: function has(key){
        return !!getEntry(this, key);
      }
    });
    if(DESCRIPTORS)dP(C.prototype, 'size', {
      get: function(){
        return defined(this[SIZE]);
      }
    });
    return C;
  },
  def: function(that, key, value){
    var entry = getEntry(that, key)
      , prev, index;
    // change existing entry
    if(entry){
      entry.v = value;
    // create new entry
    } else {
      that._l = entry = {
        i: index = fastKey(key, true), // <- index
        k: key,                        // <- key
        v: value,                      // <- value
        p: prev = that._l,             // <- previous entry
        n: undefined,                  // <- next entry
        r: false                       // <- removed
      };
      if(!that._f)that._f = entry;
      if(prev)prev.n = entry;
      that[SIZE]++;
      // add to index
      if(index !== 'F')that._i[index] = entry;
    } return that;
  },
  getEntry: getEntry,
  setStrong: function(C, NAME, IS_MAP){
    // add .keys, .values, .entries, [@@iterator]
    // 23.1.3.4, 23.1.3.8, 23.1.3.11, 23.1.3.12, 23.2.3.5, 23.2.3.8, 23.2.3.10, 23.2.3.11
    $iterDefine(C, NAME, function(iterated, kind){
      this._t = iterated;  // target
      this._k = kind;      // kind
      this._l = undefined; // previous
    }, function(){
      var that  = this
        , kind  = that._k
        , entry = that._l;
      // revert to the last existing entry
      while(entry && entry.r)entry = entry.p;
      // get next entry
      if(!that._t || !(that._l = entry = entry ? entry.n : that._t._f)){
        // or finish the iteration
        that._t = undefined;
        return step(1);
      }
      // return step by kind
      if(kind == 'keys'  )return step(0, entry.k);
      if(kind == 'values')return step(0, entry.v);
      return step(0, [entry.k, entry.v]);
    }, IS_MAP ? 'entries' : 'values' , !IS_MAP, true);

    // add [@@species], 23.1.2.2, 23.2.2.2
    setSpecies(NAME);
  }
};

/***/ }),
/* 115 */
/***/ (function(module, exports, __webpack_require__) {

// https://github.com/DavidBruant/Map-Set.prototype.toJSON
var classof = __webpack_require__(32)
  , from    = __webpack_require__(109);
module.exports = function(NAME){
  return function toJSON(){
    if(classof(this) != NAME)throw TypeError(NAME + "#toJSON isn't generic");
    return from(this);
  };
};

/***/ }),
/* 116 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var global         = __webpack_require__(2)
  , $export        = __webpack_require__(8)
  , meta           = __webpack_require__(45)
  , fails          = __webpack_require__(15)
  , hide           = __webpack_require__(11)
  , redefineAll    = __webpack_require__(49)
  , forOf          = __webpack_require__(34)
  , anInstance     = __webpack_require__(40)
  , isObject       = __webpack_require__(17)
  , setToStringTag = __webpack_require__(26)
  , dP             = __webpack_require__(4).f
  , each           = __webpack_require__(111)(0)
  , DESCRIPTORS    = __webpack_require__(6);

module.exports = function(NAME, wrapper, methods, common, IS_MAP, IS_WEAK){
  var Base  = global[NAME]
    , C     = Base
    , ADDER = IS_MAP ? 'set' : 'add'
    , proto = C && C.prototype
    , O     = {};
  if(!DESCRIPTORS || typeof C != 'function' || !(IS_WEAK || proto.forEach && !fails(function(){
    new C().entries().next();
  }))){
    // create collection constructor
    C = common.getConstructor(wrapper, NAME, IS_MAP, ADDER);
    redefineAll(C.prototype, methods);
    meta.NEED = true;
  } else {
    C = wrapper(function(target, iterable){
      anInstance(target, C, NAME, '_c');
      target._c = new Base;
      if(iterable != undefined)forOf(iterable, IS_MAP, target[ADDER], target);
    });
    each('add,clear,delete,forEach,get,has,set,keys,values,entries,toJSON'.split(','),function(KEY){
      var IS_ADDER = KEY == 'add' || KEY == 'set';
      if(KEY in proto && !(IS_WEAK && KEY == 'clear'))hide(C.prototype, KEY, function(a, b){
        anInstance(this, C, KEY);
        if(!IS_ADDER && IS_WEAK && !isObject(a))return KEY == 'get' ? undefined : false;
        var result = this._c[KEY](a === 0 ? 0 : a, b);
        return IS_ADDER ? this : result;
      });
    });
    if('size' in proto)dP(C.prototype, 'size', {
      get: function(){
        return this._c.size;
      }
    });
  }

  setToStringTag(C, NAME);

  O[NAME] = C;
  $export($export.G + $export.W + $export.F, O);

  if(!IS_WEAK)common.setStrong(C, NAME, IS_MAP);

  return C;
};

/***/ }),
/* 117 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var $defineProperty = __webpack_require__(4)
  , createDesc      = __webpack_require__(25);

module.exports = function(object, index, value){
  if(index in object)$defineProperty.f(object, index, createDesc(0, value));
  else object[index] = value;
};

/***/ }),
/* 118 */
/***/ (function(module, exports, __webpack_require__) {

// all enumerable object keys, includes symbols
var getKeys = __webpack_require__(24)
  , gOPS    = __webpack_require__(47)
  , pIE     = __webpack_require__(36);
module.exports = function(it){
  var result     = getKeys(it)
    , getSymbols = gOPS.f;
  if(getSymbols){
    var symbols = getSymbols(it)
      , isEnum  = pIE.f
      , i       = 0
      , key;
    while(symbols.length > i)if(isEnum.call(it, key = symbols[i++]))result.push(key);
  } return result;
};

/***/ }),
/* 119 */
/***/ (function(module, exports) {

// fast apply, http://jsperf.lnkit.com/fast-apply/5
module.exports = function(fn, args, that){
  var un = that === undefined;
  switch(args.length){
    case 0: return un ? fn()
                      : fn.call(that);
    case 1: return un ? fn(args[0])
                      : fn.call(that, args[0]);
    case 2: return un ? fn(args[0], args[1])
                      : fn.call(that, args[0], args[1]);
    case 3: return un ? fn(args[0], args[1], args[2])
                      : fn.call(that, args[0], args[1], args[2]);
    case 4: return un ? fn(args[0], args[1], args[2], args[3])
                      : fn.call(that, args[0], args[1], args[2], args[3]);
  } return              fn.apply(that, args);
};

/***/ }),
/* 120 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var create         = __webpack_require__(46)
  , descriptor     = __webpack_require__(25)
  , setToStringTag = __webpack_require__(26)
  , IteratorPrototype = {};

// 25.1.2.1.1 %IteratorPrototype%[@@iterator]()
__webpack_require__(11)(IteratorPrototype, __webpack_require__(1)('iterator'), function(){ return this; });

module.exports = function(Constructor, NAME, next){
  Constructor.prototype = create(IteratorPrototype, {next: descriptor(1, next)});
  setToStringTag(Constructor, NAME + ' Iterator');
};

/***/ }),
/* 121 */
/***/ (function(module, exports, __webpack_require__) {

var getKeys   = __webpack_require__(24)
  , toIObject = __webpack_require__(12);
module.exports = function(object, el){
  var O      = toIObject(object)
    , keys   = getKeys(O)
    , length = keys.length
    , index  = 0
    , key;
  while(length > index)if(O[key = keys[index++]] === el)return key;
};

/***/ }),
/* 122 */
/***/ (function(module, exports, __webpack_require__) {

var global    = __webpack_require__(2)
  , macrotask = __webpack_require__(81).set
  , Observer  = global.MutationObserver || global.WebKitMutationObserver
  , process   = global.process
  , Promise   = global.Promise
  , isNode    = __webpack_require__(23)(process) == 'process';

module.exports = function(){
  var head, last, notify;

  var flush = function(){
    var parent, fn;
    if(isNode && (parent = process.domain))parent.exit();
    while(head){
      fn   = head.fn;
      head = head.next;
      try {
        fn();
      } catch(e){
        if(head)notify();
        else last = undefined;
        throw e;
      }
    } last = undefined;
    if(parent)parent.enter();
  };

  // Node.js
  if(isNode){
    notify = function(){
      process.nextTick(flush);
    };
  // browsers with MutationObserver
  } else if(Observer){
    var toggle = true
      , node   = document.createTextNode('');
    new Observer(flush).observe(node, {characterData: true}); // eslint-disable-line no-new
    notify = function(){
      node.data = toggle = !toggle;
    };
  // environments with maybe non-completely correct, but existent Promise
  } else if(Promise && Promise.resolve){
    var promise = Promise.resolve();
    notify = function(){
      promise.then(flush);
    };
  // for other environments - macrotask based on:
  // - setImmediate
  // - MessageChannel
  // - window.postMessag
  // - onreadystatechange
  // - setTimeout
  } else {
    notify = function(){
      // strange IE + webpack dev server bug - use .call(global)
      macrotask.call(global, flush);
    };
  }

  return function(fn){
    var task = {fn: fn, next: undefined};
    if(last)last.next = task;
    if(!head){
      head = task;
      notify();
    } last = task;
  };
};

/***/ }),
/* 123 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

// 19.1.2.1 Object.assign(target, source, ...)
var getKeys  = __webpack_require__(24)
  , gOPS     = __webpack_require__(47)
  , pIE      = __webpack_require__(36)
  , toObject = __webpack_require__(27)
  , IObject  = __webpack_require__(43)
  , $assign  = Object.assign;

// should work with symbols and should have deterministic property order (V8 bug)
module.exports = !$assign || __webpack_require__(15)(function(){
  var A = {}
    , B = {}
    , S = Symbol()
    , K = 'abcdefghijklmnopqrst';
  A[S] = 7;
  K.split('').forEach(function(k){ B[k] = k; });
  return $assign({}, A)[S] != 7 || Object.keys($assign({}, B)).join('') != K;
}) ? function assign(target, source){ // eslint-disable-line no-unused-vars
  var T     = toObject(target)
    , aLen  = arguments.length
    , index = 1
    , getSymbols = gOPS.f
    , isEnum     = pIE.f;
  while(aLen > index){
    var S      = IObject(arguments[index++])
      , keys   = getSymbols ? getKeys(S).concat(getSymbols(S)) : getKeys(S)
      , length = keys.length
      , j      = 0
      , key;
    while(length > j)if(isEnum.call(S, key = keys[j++]))T[key] = S[key];
  } return T;
} : $assign;

/***/ }),
/* 124 */
/***/ (function(module, exports, __webpack_require__) {

var dP       = __webpack_require__(4)
  , anObject = __webpack_require__(10)
  , getKeys  = __webpack_require__(24);

module.exports = __webpack_require__(6) ? Object.defineProperties : function defineProperties(O, Properties){
  anObject(O);
  var keys   = getKeys(Properties)
    , length = keys.length
    , i = 0
    , P;
  while(length > i)dP.f(O, P = keys[i++], Properties[P]);
  return O;
};

/***/ }),
/* 125 */
/***/ (function(module, exports, __webpack_require__) {

// 7.3.20 SpeciesConstructor(O, defaultConstructor)
var anObject  = __webpack_require__(10)
  , aFunction = __webpack_require__(39)
  , SPECIES   = __webpack_require__(1)('species');
module.exports = function(O, D){
  var C = anObject(O).constructor, S;
  return C === undefined || (S = anObject(C)[SPECIES]) == undefined ? D : aFunction(S);
};

/***/ }),
/* 126 */
/***/ (function(module, exports, __webpack_require__) {

var toInteger = __webpack_require__(52)
  , defined   = __webpack_require__(33);
// true  -> String#at
// false -> String#codePointAt
module.exports = function(TO_STRING){
  return function(that, pos){
    var s = String(defined(that))
      , i = toInteger(pos)
      , l = s.length
      , a, b;
    if(i < 0 || i >= l)return TO_STRING ? '' : undefined;
    a = s.charCodeAt(i);
    return a < 0xd800 || a > 0xdbff || i + 1 === l || (b = s.charCodeAt(i + 1)) < 0xdc00 || b > 0xdfff
      ? TO_STRING ? s.charAt(i) : a
      : TO_STRING ? s.slice(i, i + 2) : (a - 0xd800 << 10) + (b - 0xdc00) + 0x10000;
  };
};

/***/ }),
/* 127 */
/***/ (function(module, exports, __webpack_require__) {

var toInteger = __webpack_require__(52)
  , max       = Math.max
  , min       = Math.min;
module.exports = function(index, length){
  index = toInteger(index);
  return index < 0 ? max(index + length, 0) : min(index, length);
};

/***/ }),
/* 128 */
/***/ (function(module, exports, __webpack_require__) {

var anObject = __webpack_require__(10)
  , get      = __webpack_require__(56);
module.exports = __webpack_require__(0).getIterator = function(it){
  var iterFn = get(it);
  if(typeof iterFn != 'function')throw TypeError(it + ' is not iterable!');
  return anObject(iterFn.call(it));
};

/***/ }),
/* 129 */
/***/ (function(module, exports, __webpack_require__) {

var classof   = __webpack_require__(32)
  , ITERATOR  = __webpack_require__(1)('iterator')
  , Iterators = __webpack_require__(19);
module.exports = __webpack_require__(0).isIterable = function(it){
  var O = Object(it);
  return O[ITERATOR] !== undefined
    || '@@iterator' in O
    || Iterators.hasOwnProperty(classof(O));
};

/***/ }),
/* 130 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var ctx            = __webpack_require__(14)
  , $export        = __webpack_require__(8)
  , toObject       = __webpack_require__(27)
  , call           = __webpack_require__(71)
  , isArrayIter    = __webpack_require__(69)
  , toLength       = __webpack_require__(37)
  , createProperty = __webpack_require__(117)
  , getIterFn      = __webpack_require__(56);

$export($export.S + $export.F * !__webpack_require__(72)(function(iter){ Array.from(iter); }), 'Array', {
  // 22.1.2.1 Array.from(arrayLike, mapfn = undefined, thisArg = undefined)
  from: function from(arrayLike/*, mapfn = undefined, thisArg = undefined*/){
    var O       = toObject(arrayLike)
      , C       = typeof this == 'function' ? this : Array
      , aLen    = arguments.length
      , mapfn   = aLen > 1 ? arguments[1] : undefined
      , mapping = mapfn !== undefined
      , index   = 0
      , iterFn  = getIterFn(O)
      , length, result, step, iterator;
    if(mapping)mapfn = ctx(mapfn, aLen > 2 ? arguments[2] : undefined, 2);
    // if object isn't iterable or it's array with default iterator - use simple case
    if(iterFn != undefined && !(C == Array && isArrayIter(iterFn))){
      for(iterator = iterFn.call(O), result = new C; !(step = iterator.next()).done; index++){
        createProperty(result, index, mapping ? call(iterator, mapfn, [step.value, index], true) : step.value);
      }
    } else {
      length = toLength(O.length);
      for(result = new C(length); length > index; index++){
        createProperty(result, index, mapping ? mapfn(O[index], index) : O[index]);
      }
    }
    result.length = index;
    return result;
  }
});


/***/ }),
/* 131 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var addToUnscopables = __webpack_require__(108)
  , step             = __webpack_require__(73)
  , Iterators        = __webpack_require__(19)
  , toIObject        = __webpack_require__(12);

// 22.1.3.4 Array.prototype.entries()
// 22.1.3.13 Array.prototype.keys()
// 22.1.3.29 Array.prototype.values()
// 22.1.3.30 Array.prototype[@@iterator]()
module.exports = __webpack_require__(44)(Array, 'Array', function(iterated, kind){
  this._t = toIObject(iterated); // target
  this._i = 0;                   // next index
  this._k = kind;                // kind
// 22.1.5.2.1 %ArrayIteratorPrototype%.next()
}, function(){
  var O     = this._t
    , kind  = this._k
    , index = this._i++;
  if(!O || index >= O.length){
    this._t = undefined;
    return step(1);
  }
  if(kind == 'keys'  )return step(0, index);
  if(kind == 'values')return step(0, O[index]);
  return step(0, [index, O[index]]);
}, 'values');

// argumentsList[@@iterator] is %ArrayProto_values% (9.4.4.6, 9.4.4.7)
Iterators.Arguments = Iterators.Array;

addToUnscopables('keys');
addToUnscopables('values');
addToUnscopables('entries');

/***/ }),
/* 132 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var strong = __webpack_require__(114);

// 23.1 Map Objects
module.exports = __webpack_require__(116)('Map', function(get){
  return function Map(){ return get(this, arguments.length > 0 ? arguments[0] : undefined); };
}, {
  // 23.1.3.6 Map.prototype.get(key)
  get: function get(key){
    var entry = strong.getEntry(this, key);
    return entry && entry.v;
  },
  // 23.1.3.9 Map.prototype.set(key, value)
  set: function set(key, value){
    return strong.def(this, key === 0 ? 0 : key, value);
  }
}, strong, true);

/***/ }),
/* 133 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.3.1 Object.assign(target, source)
var $export = __webpack_require__(8);

$export($export.S + $export.F, 'Object', {assign: __webpack_require__(123)});

/***/ }),
/* 134 */
/***/ (function(module, exports, __webpack_require__) {

var $export = __webpack_require__(8);
// 19.1.2.4 / 15.2.3.6 Object.defineProperty(O, P, Attributes)
$export($export.S + $export.F * !__webpack_require__(6), 'Object', {defineProperty: __webpack_require__(4).f});

/***/ }),
/* 135 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.6 Object.getOwnPropertyDescriptor(O, P)
var toIObject                 = __webpack_require__(12)
  , $getOwnPropertyDescriptor = __webpack_require__(74).f;

__webpack_require__(48)('getOwnPropertyDescriptor', function(){
  return function getOwnPropertyDescriptor(it, key){
    return $getOwnPropertyDescriptor(toIObject(it), key);
  };
});

/***/ }),
/* 136 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.7 Object.getOwnPropertyNames(O)
__webpack_require__(48)('getOwnPropertyNames', function(){
  return __webpack_require__(75).f;
});

/***/ }),
/* 137 */
/***/ (function(module, exports, __webpack_require__) {

// 19.1.2.9 Object.getPrototypeOf(O)
var toObject        = __webpack_require__(27)
  , $getPrototypeOf = __webpack_require__(77);

__webpack_require__(48)('getPrototypeOf', function(){
  return function getPrototypeOf(it){
    return $getPrototypeOf(toObject(it));
  };
});

/***/ }),
/* 138 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

var LIBRARY            = __webpack_require__(35)
  , global             = __webpack_require__(2)
  , ctx                = __webpack_require__(14)
  , classof            = __webpack_require__(32)
  , $export            = __webpack_require__(8)
  , isObject           = __webpack_require__(17)
  , aFunction          = __webpack_require__(39)
  , anInstance         = __webpack_require__(40)
  , forOf              = __webpack_require__(34)
  , speciesConstructor = __webpack_require__(125)
  , task               = __webpack_require__(81).set
  , microtask          = __webpack_require__(122)()
  , PROMISE            = 'Promise'
  , TypeError          = global.TypeError
  , process            = global.process
  , $Promise           = global[PROMISE]
  , process            = global.process
  , isNode             = classof(process) == 'process'
  , empty              = function(){ /* empty */ }
  , Internal, GenericPromiseCapability, Wrapper;

var USE_NATIVE = !!function(){
  try {
    // correct subclassing with @@species support
    var promise     = $Promise.resolve(1)
      , FakePromise = (promise.constructor = {})[__webpack_require__(1)('species')] = function(exec){ exec(empty, empty); };
    // unhandled rejections tracking support, NodeJS Promise without it fails @@species test
    return (isNode || typeof PromiseRejectionEvent == 'function') && promise.then(empty) instanceof FakePromise;
  } catch(e){ /* empty */ }
}();

// helpers
var sameConstructor = function(a, b){
  // with library wrapper special case
  return a === b || a === $Promise && b === Wrapper;
};
var isThenable = function(it){
  var then;
  return isObject(it) && typeof (then = it.then) == 'function' ? then : false;
};
var newPromiseCapability = function(C){
  return sameConstructor($Promise, C)
    ? new PromiseCapability(C)
    : new GenericPromiseCapability(C);
};
var PromiseCapability = GenericPromiseCapability = function(C){
  var resolve, reject;
  this.promise = new C(function($$resolve, $$reject){
    if(resolve !== undefined || reject !== undefined)throw TypeError('Bad Promise constructor');
    resolve = $$resolve;
    reject  = $$reject;
  });
  this.resolve = aFunction(resolve);
  this.reject  = aFunction(reject);
};
var perform = function(exec){
  try {
    exec();
  } catch(e){
    return {error: e};
  }
};
var notify = function(promise, isReject){
  if(promise._n)return;
  promise._n = true;
  var chain = promise._c;
  microtask(function(){
    var value = promise._v
      , ok    = promise._s == 1
      , i     = 0;
    var run = function(reaction){
      var handler = ok ? reaction.ok : reaction.fail
        , resolve = reaction.resolve
        , reject  = reaction.reject
        , domain  = reaction.domain
        , result, then;
      try {
        if(handler){
          if(!ok){
            if(promise._h == 2)onHandleUnhandled(promise);
            promise._h = 1;
          }
          if(handler === true)result = value;
          else {
            if(domain)domain.enter();
            result = handler(value);
            if(domain)domain.exit();
          }
          if(result === reaction.promise){
            reject(TypeError('Promise-chain cycle'));
          } else if(then = isThenable(result)){
            then.call(result, resolve, reject);
          } else resolve(result);
        } else reject(value);
      } catch(e){
        reject(e);
      }
    };
    while(chain.length > i)run(chain[i++]); // variable length - can't use forEach
    promise._c = [];
    promise._n = false;
    if(isReject && !promise._h)onUnhandled(promise);
  });
};
var onUnhandled = function(promise){
  task.call(global, function(){
    var value = promise._v
      , abrupt, handler, console;
    if(isUnhandled(promise)){
      abrupt = perform(function(){
        if(isNode){
          process.emit('unhandledRejection', value, promise);
        } else if(handler = global.onunhandledrejection){
          handler({promise: promise, reason: value});
        } else if((console = global.console) && console.error){
          console.error('Unhandled promise rejection', value);
        }
      });
      // Browsers should not trigger `rejectionHandled` event if it was handled here, NodeJS - should
      promise._h = isNode || isUnhandled(promise) ? 2 : 1;
    } promise._a = undefined;
    if(abrupt)throw abrupt.error;
  });
};
var isUnhandled = function(promise){
  if(promise._h == 1)return false;
  var chain = promise._a || promise._c
    , i     = 0
    , reaction;
  while(chain.length > i){
    reaction = chain[i++];
    if(reaction.fail || !isUnhandled(reaction.promise))return false;
  } return true;
};
var onHandleUnhandled = function(promise){
  task.call(global, function(){
    var handler;
    if(isNode){
      process.emit('rejectionHandled', promise);
    } else if(handler = global.onrejectionhandled){
      handler({promise: promise, reason: promise._v});
    }
  });
};
var $reject = function(value){
  var promise = this;
  if(promise._d)return;
  promise._d = true;
  promise = promise._w || promise; // unwrap
  promise._v = value;
  promise._s = 2;
  if(!promise._a)promise._a = promise._c.slice();
  notify(promise, true);
};
var $resolve = function(value){
  var promise = this
    , then;
  if(promise._d)return;
  promise._d = true;
  promise = promise._w || promise; // unwrap
  try {
    if(promise === value)throw TypeError("Promise can't be resolved itself");
    if(then = isThenable(value)){
      microtask(function(){
        var wrapper = {_w: promise, _d: false}; // wrap
        try {
          then.call(value, ctx($resolve, wrapper, 1), ctx($reject, wrapper, 1));
        } catch(e){
          $reject.call(wrapper, e);
        }
      });
    } else {
      promise._v = value;
      promise._s = 1;
      notify(promise, false);
    }
  } catch(e){
    $reject.call({_w: promise, _d: false}, e); // wrap
  }
};

// constructor polyfill
if(!USE_NATIVE){
  // 25.4.3.1 Promise(executor)
  $Promise = function Promise(executor){
    anInstance(this, $Promise, PROMISE, '_h');
    aFunction(executor);
    Internal.call(this);
    try {
      executor(ctx($resolve, this, 1), ctx($reject, this, 1));
    } catch(err){
      $reject.call(this, err);
    }
  };
  Internal = function Promise(executor){
    this._c = [];             // <- awaiting reactions
    this._a = undefined;      // <- checked in isUnhandled reactions
    this._s = 0;              // <- state
    this._d = false;          // <- done
    this._v = undefined;      // <- value
    this._h = 0;              // <- rejection state, 0 - default, 1 - handled, 2 - unhandled
    this._n = false;          // <- notify
  };
  Internal.prototype = __webpack_require__(49)($Promise.prototype, {
    // 25.4.5.3 Promise.prototype.then(onFulfilled, onRejected)
    then: function then(onFulfilled, onRejected){
      var reaction    = newPromiseCapability(speciesConstructor(this, $Promise));
      reaction.ok     = typeof onFulfilled == 'function' ? onFulfilled : true;
      reaction.fail   = typeof onRejected == 'function' && onRejected;
      reaction.domain = isNode ? process.domain : undefined;
      this._c.push(reaction);
      if(this._a)this._a.push(reaction);
      if(this._s)notify(this, false);
      return reaction.promise;
    },
    // 25.4.5.1 Promise.prototype.catch(onRejected)
    'catch': function(onRejected){
      return this.then(undefined, onRejected);
    }
  });
  PromiseCapability = function(){
    var promise  = new Internal;
    this.promise = promise;
    this.resolve = ctx($resolve, promise, 1);
    this.reject  = ctx($reject, promise, 1);
  };
}

$export($export.G + $export.W + $export.F * !USE_NATIVE, {Promise: $Promise});
__webpack_require__(26)($Promise, PROMISE);
__webpack_require__(80)(PROMISE);
Wrapper = __webpack_require__(0)[PROMISE];

// statics
$export($export.S + $export.F * !USE_NATIVE, PROMISE, {
  // 25.4.4.5 Promise.reject(r)
  reject: function reject(r){
    var capability = newPromiseCapability(this)
      , $$reject   = capability.reject;
    $$reject(r);
    return capability.promise;
  }
});
$export($export.S + $export.F * (LIBRARY || !USE_NATIVE), PROMISE, {
  // 25.4.4.6 Promise.resolve(x)
  resolve: function resolve(x){
    // instanceof instead of internal slot check because we should fix it without replacement native Promise core
    if(x instanceof $Promise && sameConstructor(x.constructor, this))return x;
    var capability = newPromiseCapability(this)
      , $$resolve  = capability.resolve;
    $$resolve(x);
    return capability.promise;
  }
});
$export($export.S + $export.F * !(USE_NATIVE && __webpack_require__(72)(function(iter){
  $Promise.all(iter)['catch'](empty);
})), PROMISE, {
  // 25.4.4.1 Promise.all(iterable)
  all: function all(iterable){
    var C          = this
      , capability = newPromiseCapability(C)
      , resolve    = capability.resolve
      , reject     = capability.reject;
    var abrupt = perform(function(){
      var values    = []
        , index     = 0
        , remaining = 1;
      forOf(iterable, false, function(promise){
        var $index        = index++
          , alreadyCalled = false;
        values.push(undefined);
        remaining++;
        C.resolve(promise).then(function(value){
          if(alreadyCalled)return;
          alreadyCalled  = true;
          values[$index] = value;
          --remaining || resolve(values);
        }, reject);
      });
      --remaining || resolve(values);
    });
    if(abrupt)reject(abrupt.error);
    return capability.promise;
  },
  // 25.4.4.4 Promise.race(iterable)
  race: function race(iterable){
    var C          = this
      , capability = newPromiseCapability(C)
      , reject     = capability.reject;
    var abrupt = perform(function(){
      forOf(iterable, false, function(promise){
        C.resolve(promise).then(capability.resolve, reject);
      });
    });
    if(abrupt)reject(abrupt.error);
    return capability.promise;
  }
});

/***/ }),
/* 139 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";

// ECMAScript 6 symbols shim
var global         = __webpack_require__(2)
  , has            = __webpack_require__(16)
  , DESCRIPTORS    = __webpack_require__(6)
  , $export        = __webpack_require__(8)
  , redefine       = __webpack_require__(79)
  , META           = __webpack_require__(45).KEY
  , $fails         = __webpack_require__(15)
  , shared         = __webpack_require__(51)
  , setToStringTag = __webpack_require__(26)
  , uid            = __webpack_require__(38)
  , wks            = __webpack_require__(1)
  , wksExt         = __webpack_require__(55)
  , wksDefine      = __webpack_require__(54)
  , keyOf          = __webpack_require__(121)
  , enumKeys       = __webpack_require__(118)
  , isArray        = __webpack_require__(70)
  , anObject       = __webpack_require__(10)
  , toIObject      = __webpack_require__(12)
  , toPrimitive    = __webpack_require__(53)
  , createDesc     = __webpack_require__(25)
  , _create        = __webpack_require__(46)
  , gOPNExt        = __webpack_require__(75)
  , $GOPD          = __webpack_require__(74)
  , $DP            = __webpack_require__(4)
  , $keys          = __webpack_require__(24)
  , gOPD           = $GOPD.f
  , dP             = $DP.f
  , gOPN           = gOPNExt.f
  , $Symbol        = global.Symbol
  , $JSON          = global.JSON
  , _stringify     = $JSON && $JSON.stringify
  , PROTOTYPE      = 'prototype'
  , HIDDEN         = wks('_hidden')
  , TO_PRIMITIVE   = wks('toPrimitive')
  , isEnum         = {}.propertyIsEnumerable
  , SymbolRegistry = shared('symbol-registry')
  , AllSymbols     = shared('symbols')
  , OPSymbols      = shared('op-symbols')
  , ObjectProto    = Object[PROTOTYPE]
  , USE_NATIVE     = typeof $Symbol == 'function'
  , QObject        = global.QObject;
// Don't use setters in Qt Script, https://github.com/zloirock/core-js/issues/173
var setter = !QObject || !QObject[PROTOTYPE] || !QObject[PROTOTYPE].findChild;

// fallback for old Android, https://code.google.com/p/v8/issues/detail?id=687
var setSymbolDesc = DESCRIPTORS && $fails(function(){
  return _create(dP({}, 'a', {
    get: function(){ return dP(this, 'a', {value: 7}).a; }
  })).a != 7;
}) ? function(it, key, D){
  var protoDesc = gOPD(ObjectProto, key);
  if(protoDesc)delete ObjectProto[key];
  dP(it, key, D);
  if(protoDesc && it !== ObjectProto)dP(ObjectProto, key, protoDesc);
} : dP;

var wrap = function(tag){
  var sym = AllSymbols[tag] = _create($Symbol[PROTOTYPE]);
  sym._k = tag;
  return sym;
};

var isSymbol = USE_NATIVE && typeof $Symbol.iterator == 'symbol' ? function(it){
  return typeof it == 'symbol';
} : function(it){
  return it instanceof $Symbol;
};

var $defineProperty = function defineProperty(it, key, D){
  if(it === ObjectProto)$defineProperty(OPSymbols, key, D);
  anObject(it);
  key = toPrimitive(key, true);
  anObject(D);
  if(has(AllSymbols, key)){
    if(!D.enumerable){
      if(!has(it, HIDDEN))dP(it, HIDDEN, createDesc(1, {}));
      it[HIDDEN][key] = true;
    } else {
      if(has(it, HIDDEN) && it[HIDDEN][key])it[HIDDEN][key] = false;
      D = _create(D, {enumerable: createDesc(0, false)});
    } return setSymbolDesc(it, key, D);
  } return dP(it, key, D);
};
var $defineProperties = function defineProperties(it, P){
  anObject(it);
  var keys = enumKeys(P = toIObject(P))
    , i    = 0
    , l = keys.length
    , key;
  while(l > i)$defineProperty(it, key = keys[i++], P[key]);
  return it;
};
var $create = function create(it, P){
  return P === undefined ? _create(it) : $defineProperties(_create(it), P);
};
var $propertyIsEnumerable = function propertyIsEnumerable(key){
  var E = isEnum.call(this, key = toPrimitive(key, true));
  if(this === ObjectProto && has(AllSymbols, key) && !has(OPSymbols, key))return false;
  return E || !has(this, key) || !has(AllSymbols, key) || has(this, HIDDEN) && this[HIDDEN][key] ? E : true;
};
var $getOwnPropertyDescriptor = function getOwnPropertyDescriptor(it, key){
  it  = toIObject(it);
  key = toPrimitive(key, true);
  if(it === ObjectProto && has(AllSymbols, key) && !has(OPSymbols, key))return;
  var D = gOPD(it, key);
  if(D && has(AllSymbols, key) && !(has(it, HIDDEN) && it[HIDDEN][key]))D.enumerable = true;
  return D;
};
var $getOwnPropertyNames = function getOwnPropertyNames(it){
  var names  = gOPN(toIObject(it))
    , result = []
    , i      = 0
    , key;
  while(names.length > i){
    if(!has(AllSymbols, key = names[i++]) && key != HIDDEN && key != META)result.push(key);
  } return result;
};
var $getOwnPropertySymbols = function getOwnPropertySymbols(it){
  var IS_OP  = it === ObjectProto
    , names  = gOPN(IS_OP ? OPSymbols : toIObject(it))
    , result = []
    , i      = 0
    , key;
  while(names.length > i){
    if(has(AllSymbols, key = names[i++]) && (IS_OP ? has(ObjectProto, key) : true))result.push(AllSymbols[key]);
  } return result;
};

// 19.4.1.1 Symbol([description])
if(!USE_NATIVE){
  $Symbol = function Symbol(){
    if(this instanceof $Symbol)throw TypeError('Symbol is not a constructor!');
    var tag = uid(arguments.length > 0 ? arguments[0] : undefined);
    var $set = function(value){
      if(this === ObjectProto)$set.call(OPSymbols, value);
      if(has(this, HIDDEN) && has(this[HIDDEN], tag))this[HIDDEN][tag] = false;
      setSymbolDesc(this, tag, createDesc(1, value));
    };
    if(DESCRIPTORS && setter)setSymbolDesc(ObjectProto, tag, {configurable: true, set: $set});
    return wrap(tag);
  };
  redefine($Symbol[PROTOTYPE], 'toString', function toString(){
    return this._k;
  });

  $GOPD.f = $getOwnPropertyDescriptor;
  $DP.f   = $defineProperty;
  __webpack_require__(76).f = gOPNExt.f = $getOwnPropertyNames;
  __webpack_require__(36).f  = $propertyIsEnumerable;
  __webpack_require__(47).f = $getOwnPropertySymbols;

  if(DESCRIPTORS && !__webpack_require__(35)){
    redefine(ObjectProto, 'propertyIsEnumerable', $propertyIsEnumerable, true);
  }

  wksExt.f = function(name){
    return wrap(wks(name));
  }
}

$export($export.G + $export.W + $export.F * !USE_NATIVE, {Symbol: $Symbol});

for(var symbols = (
  // 19.4.2.2, 19.4.2.3, 19.4.2.4, 19.4.2.6, 19.4.2.8, 19.4.2.9, 19.4.2.10, 19.4.2.11, 19.4.2.12, 19.4.2.13, 19.4.2.14
  'hasInstance,isConcatSpreadable,iterator,match,replace,search,species,split,toPrimitive,toStringTag,unscopables'
).split(','), i = 0; symbols.length > i; )wks(symbols[i++]);

for(var symbols = $keys(wks.store), i = 0; symbols.length > i; )wksDefine(symbols[i++]);

$export($export.S + $export.F * !USE_NATIVE, 'Symbol', {
  // 19.4.2.1 Symbol.for(key)
  'for': function(key){
    return has(SymbolRegistry, key += '')
      ? SymbolRegistry[key]
      : SymbolRegistry[key] = $Symbol(key);
  },
  // 19.4.2.5 Symbol.keyFor(sym)
  keyFor: function keyFor(key){
    if(isSymbol(key))return keyOf(SymbolRegistry, key);
    throw TypeError(key + ' is not a symbol!');
  },
  useSetter: function(){ setter = true; },
  useSimple: function(){ setter = false; }
});

$export($export.S + $export.F * !USE_NATIVE, 'Object', {
  // 19.1.2.2 Object.create(O [, Properties])
  create: $create,
  // 19.1.2.4 Object.defineProperty(O, P, Attributes)
  defineProperty: $defineProperty,
  // 19.1.2.3 Object.defineProperties(O, Properties)
  defineProperties: $defineProperties,
  // 19.1.2.6 Object.getOwnPropertyDescriptor(O, P)
  getOwnPropertyDescriptor: $getOwnPropertyDescriptor,
  // 19.1.2.7 Object.getOwnPropertyNames(O)
  getOwnPropertyNames: $getOwnPropertyNames,
  // 19.1.2.8 Object.getOwnPropertySymbols(O)
  getOwnPropertySymbols: $getOwnPropertySymbols
});

// 24.3.2 JSON.stringify(value [, replacer [, space]])
$JSON && $export($export.S + $export.F * (!USE_NATIVE || $fails(function(){
  var S = $Symbol();
  // MS Edge converts symbol values to JSON as {}
  // WebKit converts symbol values to JSON as null
  // V8 throws on boxed symbols
  return _stringify([S]) != '[null]' || _stringify({a: S}) != '{}' || _stringify(Object(S)) != '{}';
})), 'JSON', {
  stringify: function stringify(it){
    if(it === undefined || isSymbol(it))return; // IE8 returns string on undefined
    var args = [it]
      , i    = 1
      , replacer, $replacer;
    while(arguments.length > i)args.push(arguments[i++]);
    replacer = args[1];
    if(typeof replacer == 'function')$replacer = replacer;
    if($replacer || !isArray(replacer))replacer = function(key, value){
      if($replacer)value = $replacer.call(this, key, value);
      if(!isSymbol(value))return value;
    };
    args[1] = replacer;
    return _stringify.apply($JSON, args);
  }
});

// 19.4.3.4 Symbol.prototype[@@toPrimitive](hint)
$Symbol[PROTOTYPE][TO_PRIMITIVE] || __webpack_require__(11)($Symbol[PROTOTYPE], TO_PRIMITIVE, $Symbol[PROTOTYPE].valueOf);
// 19.4.3.5 Symbol.prototype[@@toStringTag]
setToStringTag($Symbol, 'Symbol');
// 20.2.1.9 Math[@@toStringTag]
setToStringTag(Math, 'Math', true);
// 24.3.3 JSON[@@toStringTag]
setToStringTag(global.JSON, 'JSON', true);

/***/ }),
/* 140 */
/***/ (function(module, exports, __webpack_require__) {

// https://github.com/DavidBruant/Map-Set.prototype.toJSON
var $export  = __webpack_require__(8);

$export($export.P + $export.R, 'Map', {toJSON: __webpack_require__(115)('Map')});

/***/ }),
/* 141 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(54)('asyncIterator');

/***/ }),
/* 142 */
/***/ (function(module, exports, __webpack_require__) {

__webpack_require__(54)('observable');

/***/ }),
/* 143 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
Object.defineProperty(__webpack_exports__, "__esModule", { value: true });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__src_App_fs__ = __webpack_require__(82);
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "state2", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["a"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "state", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["b"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "n", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["c"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "a", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["d"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "b", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["e"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "colors", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["f"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "draw", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["g"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "AgentEvent", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["h"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "game", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["i"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "touchEvents", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["j"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "setUpTouchEvents", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["k"]; });
/* harmony namespace reexport (by provided) */ __webpack_require__.d(__webpack_exports__, "init", function() { return __WEBPACK_IMPORTED_MODULE_0__src_App_fs__["l"]; });


/***/ }),
/* 144 */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from__ = __webpack_require__(31);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__ = __webpack_require__(5);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass__ = __webpack_require__(7);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__ = __webpack_require__(9);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__ = __webpack_require__(3);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__ = __webpack_require__(59);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__node_modules_fable_core_Choice__ = __webpack_require__(30);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "b", function() { return Array2D; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return Game; });








var Array2D = function (__exports) {
    var Array2D = __exports.Array2D = function () {
        function Array2D(base1, base2, arr) {
            __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, Array2D);

            this.base1 = base1 | 0;
            this.base2 = base2 | 0;
            this.arr = arr;
        }

        __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(Array2D, [{
            key: __WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["a" /* default */].reflection,
            value: function value() {
                return {
                    type: "game.Array2D.Array2D",
                    interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
                    properties: {
                        base1: "number",
                        base2: "number",
                        arr: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["h" /* Array */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["h" /* Array */])(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["i" /* GenericParam */])("a")))
                    }
                };
            }
        }, {
            key: "Equals",
            value: function Equals(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["j" /* equalsRecords */])(this, other);
            }
        }, {
            key: "CompareTo",
            value: function CompareTo(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["k" /* compareRecords */])(this, other) | 0;
            }
        }, {
            key: "get_Item",
            value: function get_Item(i1, i2) {
                return this.arr[i1][i2];
            }
        }, {
            key: "set_Item",
            value: function set_Item(i1, i2, value) {
                this.arr[i1][i2] = value;
            }
        }, {
            key: "Length1",
            get: function get() {
                return this.base1 | 0;
            }
        }, {
            key: "Length2",
            get: function get() {
                return this.base2 | 0;
            }
        }]);

        return Array2D;
    }();

    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["b" /* setType */])("game.Array2D.Array2D", Array2D);

    var exists = __exports.exists = function (f, arr) {
        var exists_ = function exists_(i, j) {
            exists_: while (true) {
                var matchValue = f(arr.get_Item(i, j));

                if (matchValue) {
                    return true;
                } else if (j < arr.base2 - 1) {
                    i = i;
                    j = j + 1;
                    continue exists_;
                } else if (i < arr.base1 - 1) {
                    i = i + 1;
                    j = 0;
                    continue exists_;
                } else {
                    return false;
                }
            }
        };

        return exists_(0, 0);
    };

    var set = __exports.set = function (arr, a, b, v) {
        arr.set_Item(a, b, v);
    };

    var init = __exports.init = function (a, b, f) {
        return new Array2D(a, b, __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from___default()(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["b" /* delay */])(function () {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["a" /* map */])(function (i) {
                return __WEBPACK_IMPORTED_MODULE_0_babel_runtime_core_js_array_from___default()(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["b" /* delay */])(function () {
                    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["a" /* map */])(function (j) {
                        return f(i, j);
                    }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["c" /* range */])(0, b - 1));
                }));
            }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["c" /* range */])(0, a - 1));
        })));
    };

    var create = __exports.create = function (a, b, x) {
        return init(a, b, function (_arg2, _arg1) {
            return x;
        });
    };

    var mapi = __exports.mapi = function (f, arr) {
        return init(arr.base1, arr.base2, function (i, j) {
            return f(i, j, arr.get_Item(i, j));
        });
    };

    var map = __exports.map = function (f) {
        var f_1 = function f_1(_arg2, _arg1, x) {
            return f(x);
        };

        return function (arr) {
            return mapi(f_1, arr);
        };
    };

    var toSeq = __exports.toSeq = function (arr) {
        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["b" /* delay */])(function () {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["d" /* collect */])(function (i) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["a" /* map */])(function (j) {
                    return [i, j, arr.get_Item(i, j)];
                }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["c" /* range */])(0, arr.base2 - 1));
            }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["c" /* range */])(0, arr.base1 - 1));
        });
    };

    var iteri = __exports.iteri = function (f, arr) {
        for (var i = 0; i <= arr.base1 - 1; i++) {
            for (var j = 0; j <= arr.base2 - 1; j++) {
                f(i, j, arr.get_Item(i, j));
            }
        }
    };

    var ofList = __exports.ofList = function (l) {
        var x = l.length | 0;
        var y = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["e" /* item */])(0, l).length | 0;
        return init(y, x, function (x_1, y_1) {
            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["e" /* item */])(x_1, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["e" /* item */])(y_1, l));
        });
    };

    return __exports;
}({});
var Game = function (__exports) {
    var BoardSize = __exports.BoardSize = function (__exports) {
        var width = __exports.width = 10;
        var height = __exports.height = 15;
        return __exports;
    }({});

    var Block = __exports.Block = function () {
        function Block(color) {
            __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, Block);

            this.Color = color | 0;
        }

        __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(Block, [{
            key: __WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["a" /* default */].reflection,
            value: function value() {
                return {
                    type: "game.Game.Block",
                    interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
                    properties: {
                        Color: "number"
                    }
                };
            }
        }, {
            key: "Equals",
            value: function Equals(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["j" /* equalsRecords */])(this, other);
            }
        }, {
            key: "CompareTo",
            value: function CompareTo(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["k" /* compareRecords */])(this, other) | 0;
            }
        }]);

        return Block;
    }();

    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["b" /* setType */])("game.Game.Block", Block);

    var State = __exports.State = function () {
        function State(blocks, points) {
            __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, State);

            this.Blocks = blocks;
            this.Points = points | 0;
        }

        __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(State, [{
            key: __WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["a" /* default */].reflection,
            value: function value() {
                return {
                    type: "game.Game.State",
                    interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
                    properties: {
                        Blocks: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["l" /* makeGeneric */])(Array2D.Array2D, {
                            a: __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["m" /* Tuple */])(["boolean", __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["n" /* Option */])("number")])
                        }),
                        Points: "number"
                    }
                };
            }
        }, {
            key: "Equals",
            value: function Equals(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["j" /* equalsRecords */])(this, other);
            }
        }, {
            key: "CompareTo",
            value: function CompareTo(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["k" /* compareRecords */])(this, other) | 0;
            }
        }]);

        return State;
    }();

    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["b" /* setType */])("game.Game.State", State);

    var KeyPressed = __exports.KeyPressed = function () {
        function KeyPressed(up, down, left, right) {
            __WEBPACK_IMPORTED_MODULE_1_babel_runtime_helpers_classCallCheck___default()(this, KeyPressed);

            this.Up = up;
            this.Down = down;
            this.Left = left;
            this.Right = right;
        }

        __WEBPACK_IMPORTED_MODULE_2_babel_runtime_helpers_createClass___default()(KeyPressed, [{
            key: __WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["a" /* default */].reflection,
            value: function value() {
                return {
                    type: "game.Game.KeyPressed",
                    interfaces: ["FSharpRecord", "System.IEquatable", "System.IComparable"],
                    properties: {
                        Up: "boolean",
                        Down: "boolean",
                        Left: "boolean",
                        Right: "boolean"
                    }
                };
            }
        }, {
            key: "Equals",
            value: function Equals(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["j" /* equalsRecords */])(this, other);
            }
        }, {
            key: "CompareTo",
            value: function CompareTo(other) {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["k" /* compareRecords */])(this, other) | 0;
            }
        }, {
            key: "Any",
            value: function Any() {
                if ((this.Up ? true : this.Down) ? true : this.Left) {
                    return true;
                } else {
                    return this.Right;
                }
            }
        }], [{
            key: "Default",
            get: function get() {
                return new KeyPressed(false, false, false, false);
            }
        }]);

        return KeyPressed;
    }();

    __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_3__node_modules_fable_core_Symbol__["b" /* setType */])("game.Game.KeyPressed", KeyPressed);

    var _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_ = __exports["|IsLeftPressed|IsRightPressed|IsUpPressed|IsDownPressed|Nothing|"] = function (kp) {
        if (kp.Up) {
            return new __WEBPACK_IMPORTED_MODULE_6__node_modules_fable_core_Choice__["a" /* default */](2, null);
        } else if (kp.Down) {
            return new __WEBPACK_IMPORTED_MODULE_6__node_modules_fable_core_Choice__["a" /* default */](3, null);
        } else if (kp.Left) {
            return new __WEBPACK_IMPORTED_MODULE_6__node_modules_fable_core_Choice__["a" /* default */](0, null);
        } else if (kp.Right) {
            return new __WEBPACK_IMPORTED_MODULE_6__node_modules_fable_core_Choice__["a" /* default */](1, null);
        } else {
            return new __WEBPACK_IMPORTED_MODULE_6__node_modules_fable_core_Choice__["a" /* default */](4, null);
        }
    };

    var empty = __exports.empty = function () {
        return [false, null];
    };

    var initState = __exports.initState = new State(Array2D.create(10, 15, empty()), 0);

    var score = __exports.score = function (state) {
        var width_1 = state.Blocks.Length1 | 0;
        var height_1 = state.Blocks.Length2 | 0;
        var result = Array2D.init(width_1, height_1, function (_arg2, _arg1) {
            return [false, null];
        });
        var moveUp = 0;

        var _loop = function _loop(y) {
            var fullRow = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["f" /* forAll */])(function (x) {
                return x;
            }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["b" /* delay */])(function () {
                return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["d" /* collect */])(function (x_1) {
                    var patternInput = state.Blocks.get_Item(x_1, y);
                    return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["g" /* singleton */])(!patternInput[0] ? patternInput[1] != null : false);
                }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["h" /* rangeStep */])(width_1 - 1, -1, 0));
            }));

            if (fullRow) {
                moveUp = moveUp + 1 | 0;
            }

            if (!fullRow) {
                var y_ = y + moveUp | 0;

                if (y_ >= 0) {
                    for (var x_2 = width_1 - 1; x_2 >= 0; x_2--) {
                        Array2D.set(result, x_2, y_, state.Blocks.get_Item(x_2, y));
                    }
                }
            }
        };

        for (var y = height_1 - 1; y >= 0; y--) {
            _loop(y);
        }

        return new State(result, state.Points + moveUp);
    };

    var canApplyKeyboardTransition = __exports.canApplyKeyboardTransition = function (keyPressed, state) {
        var width_1 = state.Blocks.Length1 | 0;
        var height_1 = state.Blocks.Length2 | 0;

        if (!keyPressed.Any()) {
            return true;
        } else {
            var isValidMove = function isValidMove(x, y) {
                var activePatternResult211 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                if (activePatternResult211.tag === 0) {
                    if (x > 0) {
                        if (state.Blocks.get_Item(x - 1, y)[0]) {
                            return true;
                        } else {
                            return state.Blocks.get_Item(x - 1, y)[1] == null;
                        }
                    } else {
                        return false;
                    }
                } else if (activePatternResult211.tag === 1) {
                    if (x < width_1 - 1) {
                        if (state.Blocks.get_Item(x + 1, y)[0]) {
                            return true;
                        } else {
                            return state.Blocks.get_Item(x + 1, y)[1] == null;
                        }
                    } else {
                        return false;
                    }
                } else if (activePatternResult211.tag === 3) {
                    if (y < height_1 - 1) {
                        if (state.Blocks.get_Item(x, y + 1)[0]) {
                            return true;
                        } else {
                            return state.Blocks.get_Item(x, y + 1)[1] == null;
                        }
                    } else {
                        return false;
                    }
                } else {
                    return true;
                }
            };

            return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["f" /* forAll */])(function (tupledArg) {
                return isValidMove(tupledArg[0], tupledArg[1]);
            }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["i" /* filter */])(function (tupledArg_1) {
                return tupledArg_1[2][0];
            }, Array2D.toSeq(state.Blocks)));
        }
    };

    var activeBlockSpace = __exports.activeBlockSpace = function (state) {
        var min = function min(a, b) {
            return a != null ? __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["d" /* compare */])(a, b) < 0 ? a : b : b;
        };

        var max = function max(a_1, b_1) {
            return a_1 != null ? __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["d" /* compare */])(a_1, b_1) > 0 ? a_1 : b_1 : b_1;
        };

        return function (_arg1) {
            var $var1 = _arg1[0] != null ? _arg1[1] != null ? _arg1[2] != null ? _arg1[3] != null ? [0, _arg1[1], _arg1[3], _arg1[0], _arg1[2]] : [1] : [1] : [1] : [1];

            switch ($var1[0]) {
                case 0:
                    return [[$var1[3], $var1[1]], [$var1[4], $var1[2]]];

                case 1:
                    return null;
            }
        }(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["j" /* fold */])(function (tupledArg, tupledArg_1) {
            return [min(tupledArg[0], tupledArg_1[0]), max(tupledArg[1], tupledArg_1[0]), min(tupledArg[2], tupledArg_1[1]), max(tupledArg[3], tupledArg_1[1])];
        }, [null, null, null, null], __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["i" /* filter */])(function (tupledArg_2) {
            return tupledArg_2[2][0];
        }, Array2D.toSeq(state.Blocks))));
    };

    var canRotate = __exports.canRotate = function (state) {
        var width_1 = state.Blocks.Length1 | 0;
        var height_1 = state.Blocks.Length2 | 0;
        var p = activeBlockSpace(state);

        if (p != null) {
            var minY = p[1][0] | 0;
            var minX = p[0][0] | 0;
            var maxY = p[1][1] | 0;
            var maxX = p[0][1] | 0;
            var distance = (maxX - minX > maxY - minY ? maxX - minX : maxY - minY) + 1 | 0;
            var centralX = ~~((minX + maxX) / 2) | 0;
            var centralY = ~~((minY + maxY) / 2) | 0;
            var patternInput = [centralX - ~~(distance / 2), centralX + ~~(distance / 2), centralY - ~~(distance / 2), centralY + ~~(distance / 2)];
            return function (_arg1) {
                var $var2 = _arg1 ? [1] : (((minX > 0 ? minY > 0 : false) ? maxX < width_1 : false) ? maxY < height_1 : false) ? [0] : [1];

                switch ($var2[0]) {
                    case 0:
                        return [centralX, centralY, distance];

                    case 1:
                        return null;
                }
            }(__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["k" /* exists */])(function (tupledArg) {
                return (((tupledArg[0] >= patternInput[0] ? tupledArg[0] <= patternInput[1] : false) ? tupledArg[1] >= patternInput[2] : false) ? tupledArg[1] <= patternInput[3] : false) ? !tupledArg[2][0] ? tupledArg[2][1] != null : false : false;
            }, Array2D.toSeq(state.Blocks)));
        } else {
            throw new Error("/Users/kevin/Projects/tetris/fable/src/Logic.fs", 144, 12);
        }
    };

    var rotate = __exports.rotate = function (state) {
        var matchValue = canRotate(state);

        if (matchValue != null) {
            var distance = matchValue[2] | 0;
            var centralY = matchValue[1] | 0;
            var centralX = matchValue[0] | 0;
            var patternInput = [centralX - ~~(distance / 2), centralX + ~~(distance / 2), centralY - ~~(distance / 2), centralY + ~~(distance / 2)];

            var _blocks = Array2D.mapi(function (x, y, v) {
                if (((x >= patternInput[0] ? x <= patternInput[1] : false) ? y >= patternInput[2] : false) ? y <= patternInput[3] : false) {
                    var x_ = centralX - (y - centralY) | 0;
                    var y_ = centralY + (x - centralX) | 0;
                    return state.Blocks.get_Item(x_, y_);
                } else {
                    return v;
                }
            }, state.Blocks);

            return new State(_blocks, state.Points);
        } else {
            return state;
        }
    };

    var applyKeyboardTransition = __exports.applyKeyboardTransition = function (keyPressed, state) {
        var width_1 = state.Blocks.Length1 | 0;
        var height_1 = state.Blocks.Length2 | 0;

        if (keyPressed.Any() ? canApplyKeyboardTransition(keyPressed, state) : false) {
            var _blocks2 = Array2D.mapi(function (x, y, _arg1) {
                var x_ = void 0;

                if (width_1 - 1 < (0 > function () {
                    var activePatternResult239 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                    if (activePatternResult239.tag === 0) {
                        return x + 1 | 0;
                    } else if (activePatternResult239.tag === 1) {
                        return x - 1 | 0;
                    } else {
                        return x | 0;
                    }
                }() ? 0 : function () {
                    var activePatternResult239 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                    if (activePatternResult239.tag === 0) {
                        return x + 1 | 0;
                    } else if (activePatternResult239.tag === 1) {
                        return x - 1 | 0;
                    } else {
                        return x | 0;
                    }
                }())) {
                    x_ = width_1 - 1 | 0;
                } else if (0 > function () {
                    var activePatternResult239 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                    if (activePatternResult239.tag === 0) {
                        return x + 1 | 0;
                    } else if (activePatternResult239.tag === 1) {
                        return x - 1 | 0;
                    } else {
                        return x | 0;
                    }
                }()) {
                    x_ = 0;
                } else {
                    var activePatternResult239 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                    if (activePatternResult239.tag === 0) {
                        x_ = x + 1 | 0;
                    } else if (activePatternResult239.tag === 1) {
                        x_ = x - 1 | 0;
                    } else {
                        x_ = x | 0;
                    }
                }

                var y_ = void 0;

                if (0 > function () {
                    var activePatternResult240 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                    if (activePatternResult240.tag === 3) {
                        return y - 1 | 0;
                    } else {
                        return y | 0;
                    }
                }()) {
                    y_ = 0;
                } else {
                    var activePatternResult240 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                    if (activePatternResult240.tag === 3) {
                        y_ = y - 1 | 0;
                    } else {
                        y_ = y | 0;
                    }
                }

                var patternInput = state.Blocks.get_Item(x_, y_);

                if (patternInput[0]) {
                    var $var3 = void 0;

                    var activePatternResult244 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                    if (activePatternResult244.tag === 0) {
                        if (x === width_1 - 1) {
                            $var3 = [0];
                        } else {
                            $var3 = [1];
                        }
                    } else {
                        $var3 = [1];
                    }

                    switch ($var3[0]) {
                        case 0:
                            return empty();

                        case 1:
                            var $var4 = void 0;

                            var activePatternResult243 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                            if (activePatternResult243.tag === 1) {
                                if (x === 0) {
                                    $var4 = [0];
                                } else {
                                    $var4 = [1];
                                }
                            } else {
                                $var4 = [1];
                            }

                            switch ($var4[0]) {
                                case 0:
                                    return empty();

                                case 1:
                                    var $var5 = void 0;

                                    var activePatternResult242 = _IsLeftPressed_IsRightPressed_IsUpPressed_IsDownPressed_Nothing_(keyPressed);

                                    if (activePatternResult242.tag === 3) {
                                        if (y === 0) {
                                            $var5 = [0];
                                        } else {
                                            $var5 = [1];
                                        }
                                    } else {
                                        $var5 = [1];
                                    }

                                    switch ($var5[0]) {
                                        case 0:
                                            return empty();

                                        case 1:
                                            return [patternInput[0], patternInput[1]];
                                    }

                            }

                    }
                } else {
                    var patternInput_1 = state.Blocks.get_Item(x, y);

                    if (patternInput_1[0]) {
                        return empty();
                    } else {
                        return [patternInput_1[0], patternInput_1[1]];
                    }
                }
            }, state.Blocks);

            var state_1 = new State(_blocks2, state.Points);

            if (keyPressed.Up) {
                return rotate(state_1);
            } else {
                return state_1;
            }
        } else {
            return state;
        }
    };

    var canMove = __exports.canMove = function (state) {
        var width_1 = state.Blocks.Length1 | 0;
        var height_1 = state.Blocks.Length2 | 0;

        var isValidMove = function isValidMove(x, y) {
            if (y === height_1 - 1) {
                return false;
            } else {
                var patternInput = state.Blocks.get_Item(x, y + 1);

                if (patternInput[0]) {
                    return true;
                } else if (patternInput[1] == null) {
                    return !patternInput[0];
                } else {
                    return false;
                }
            }
        };

        return __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["f" /* forAll */])(function (tupledArg) {
            return isValidMove(tupledArg[0], tupledArg[1]);
        }, __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_5__node_modules_fable_core_Seq__["i" /* filter */])(function (tupledArg_1) {
            return tupledArg_1[2][0];
        }, Array2D.toSeq(state.Blocks)));
    };

    var move = __exports.move = function (state) {
        if (canMove(state)) {
            return new State(Array2D.mapi(function (x, y, tupledArg) {
                if (y === 0) {
                    return empty();
                } else {
                    var patternInput = state.Blocks.get_Item(x, y - 1);

                    if (patternInput[0]) {
                        return [patternInput[0], patternInput[1]];
                    } else if (tupledArg[0]) {
                        return empty();
                    } else {
                        return [tupledArg[0], tupledArg[1]];
                    }
                }
            }, state.Blocks), state.Points);
        } else {
            return new State(Array2D.map(function (tupledArg_1) {
                return [false, tupledArg_1[1]];
            })(state.Blocks), state.Points);
        }
    };

    var blocks = __exports.blocks = [["X", "X", "XX"], ["XX", "X", "X"], [" X ", "XXX"], ["XXXX"], ["XX", "XX"], ["  X", " X"]];

    var getRandomBlock = __exports.getRandomBlock = function (rnd) {
        var str = blocks[__webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["o" /* randomNext */])(0, blocks.length)];
        var c = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["o" /* randomNext */])(0, 255) | 0;
        return Array2D.init(3, 4, function (x, y) {
            return ((str.length > x ? str[x].length > y : false) ? str[x][y] !== " " : false) ? c : null;
        });
    };

    var applyTransition = __exports.applyTransition = function (rnd, state) {
        if (!Array2D.exists(function (_arg1) {
            return _arg1[0];
        }, state.Blocks)) {
            var nextBlock = getRandomBlock(rnd);
            var middle = ~~(state.Blocks.Length1 / 2) | 0;
            var color = __webpack_require__.i(__WEBPACK_IMPORTED_MODULE_4__node_modules_fable_core_Util__["o" /* randomNext */])(0, 255) | 0;
            Array2D.iteri(function (x, y, v) {
                if (v != null) {
                    Array2D.set(state.Blocks, x + middle - 1, y, [true, v]);
                }
            }, nextBlock);
        }

        return new State(state.Blocks, state.Points);
    };

    return __exports;
}({});

/***/ })
/******/ ]);
//# sourceMappingURL=bundle.js.map