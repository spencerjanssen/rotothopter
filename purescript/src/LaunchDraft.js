function getSeedUsers(success, error){
    if(window.hasOwnProperty('seedusers')){
        success(window.seedusers);
    } else {
        setTimeout(function(){getSeedUsers(success, error);}, 10);
    }
}
exports.seedUsers = getSeedUsers;

exports.swap = function(arr){
    return function(i){
        return function(j){
            if(i >= 0 && i < arr.length && j >= 0 && j < arr.length){
                var res = arr.slice();
                res[i] = arr[j];
                res[j] = arr[i];
                return res;
            } else {
                return arr;
            }
        }
    }
};

exports.mapIndex = function(f){
    return function(arr){
        var res = [];
        for(var i = 0; i < arr.length; i++){
            res[i] = f(i)(arr[i]);
        }
        return res
    }
}

window.seedusers;
