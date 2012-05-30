package br.unb.cdt.desafioPositivo.model.acesso;

import java.util.Calendar;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.DiscriminatorValue;
import javax.persistence.Entity;
import javax.persistence.Table;

@Entity
@Table(name="TB_ACESSO_ATIVO")
@DiscriminatorValue("2")
public class AcessoAtivo extends AcessoUsuario {

	private static final long serialVersionUID = 1L;

	private static final Integer MAX_TENTATIVAS_INVALIDAS = 5;
	
	@Column(name="TENTATIVAS_INVALIDAS")
	protected Integer tentativasInvalidas;
	
	@Column(name="DATA_BLOQUEIO")
	protected Date dataBloqueio;
	
	@Column(name="ULTIMO_ACESSO")
	protected Date ultimoAcesso;
	
	@Override
	public void autenticar(boolean autenticacao) throws ExcecaoAcessoUsuario {
		if(autenticacao) {
			ultimoAcesso = Calendar.getInstance().getTime();
			tentativasInvalidas = 0;
		}
		else {
			tentativasInvalidas++;
			if(tentativasInvalidas > MAX_TENTATIVAS_INVALIDAS) {
				dataFim = Calendar.getInstance().getTime();
				AcessoUsuario acessoUsuario = new AcessoBloqueado();
				acessoUsuario.setUsuario(usuario);
				usuario.getHistoricoSituacaoAcesso().add(acessoUsuario);
			}
		}
	}

	@Override
	public void confirmarCadastro(String token) throws ExcecaoAcessoUsuario {
		throw new ExcecaoAcessoUsuario("Usuario com acesso ativo.");
	}

	@Override
	public void desbloquear() throws ExcecaoAcessoUsuario {
		throw new ExcecaoAcessoUsuario("Usuario com acesso ativo, nao sendo necessario o desbloqueio.");
	}

}
