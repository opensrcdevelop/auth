import authRequest from '@/util/authRequest'

/**
 * 登出
 * 
 * @returns 登出状态
 */
export function logoutSubmit() {
    return authRequest.post({
        url: '/logout'
    })
}