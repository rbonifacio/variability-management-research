package br.unb.cdt.desafioPositivo.action;

import org.jboss.seam.ScopeType;
import org.jboss.seam.annotations.In;
import org.jboss.seam.annotations.Logger;
import org.jboss.seam.annotations.Name;
import org.jboss.seam.annotations.Out;
import org.jboss.seam.log.Log;
import org.jboss.seam.security.Credentials;
import org.jboss.seam.security.Identity;

import br.unb.cdt.desafioPositivo.facade.DesafioPositivoFacade;
import br.unb.cdt.desafioPositivo.model.Usuario;
import br.unb.cdt.desafioPositivo.util.rest.AutenticacaoSRV;
import br.unb.cdt.desafioPositivo.util.rest.RespostaPositivo;

@Name("authenticator")
public class Authenticator
{
    @Logger private Log log;

    @In Identity identity;
    @In Credentials credentials;

    @In 
    private DesafioPositivoFacade facade;
    
    @Out(scope=ScopeType.SESSION)
    private Usuario usuarioLogado;
    
    public boolean authenticate()
    {
    	log.info("authenticating {0}", credentials.getUsername());
    	try {
			//NOTE: o mecanismo de bijecao de dependencias 
    		//disponibiliza usuario logado no contexto 
    		//da sessao.
    		usuarioLogado = facade.autenticarUsuario(credentials.getUsername(), credentials.getPassword());
			return true;
    	} catch (Exception e) {
    		e.printStackTrace();
    		return false;
		} 
    }

}
